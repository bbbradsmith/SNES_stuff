import PIL.Image

#
# Utilities
#

# save a bytearray to a file
def bin_save(filename,b):
    print("%20s %6d bytes" % (filename,len(b)))
    open(filename,"wb").write(b)

# load an image as RGB
def rgb_img_load(filename):
    return PIL.Image.open(filename).convert("RGB")

# set image palette from RGB tuples
def index_setpal(img,palette): 
    linpal = [p for tup in palette for p in tup[0:3]]
    img.putpalette(linpal)

# load and convert image to indexed palette
def index_img_load(filename, palette):
    src = PIL.Image.open(filename).convert("RGB")
    dst = PIL.Image.new("P",src.size,color=0)
    for y in range(src.size[1]):
        for x in range(src.size[0]):
            p = src.getpixel((x,y))
            mag = ((255**2)*3)+1
            mat = 0
            for i in range(len(palette)):
                m = sum([(a-b)**2 for (a,b) in zip(p,palette[i])])
                if m < mag: # better match
                    mat = i
                    mag = m
                    if m == 0: # perfect match
                        break
            dst.putpixel((x,y),mat)
    index_setpal(dst,palette)
    return dst

# extract colours from an image to make a palette (row order)
def make_pal(img,x=0,y=0,w=-1,h=-1):
    img = img.convert("RGB")
    palette = []
    if w < 0: w = img.size[0] - x
    if h < 0: h = img.size[1] - y
    for py in range(y,y+h):
        for px in range(x,x+w):
            p = img.getpixel((px,py))
            if p not in palette:
                palette.append(p)
    return palette

# convert palette to SNES 555 format
def snes_pal(palette):
    d = bytearray()
    for p in palette:
        r = p[0] >> 3
        g = p[1] >> 3
        b = p[2] >> 3
        d.append(r | ((g & 7) << 5))
        d.append((g >> 3) | (b << 2))
    return d

# convert rectangles to CHR data, planes = 1 (2bpp) or 2 (4bpp)
def chr(img,x=0,y=0,w=-1,h=-1,planes=2):
    b = bytearray()
    if w < 0: w = img.size[0] - x
    if h < 0: h = img.size[1] - y
    for ty in range(y,y+h,8):
        for tx in range(x,x+w,8):
            for tp in range(planes):
                for py in range(8):
                    b0 = 0
                    b1 = 0
                    for px in range(8):
                        p = (img.getpixel((tx+px,ty+py)) >> (tp * 2)) & 3
                        b0 = (b0 << 1) | (p & 1)
                        b1 = (b1 << 1) | ((p >> 1) & 1)
                    b.append(b0)
                    b.append(b1)
    return b

# box filtered scale by 50%
def scalehalf(src):
    dst = PIL.Image.new("RGB",(src.size[0]//2,src.size[1]//2))
    for y in range(0,src.size[1],2):
        for x in range(0,src.size[0],2):
            pd = [0,0,0]
            for sy in range(2):
                for sx in range(2):
                    p = src.getpixel((x+sx,y+sy))
                    for i in range(3): pd[i] += p[i]
            for i in range(3): pd[i] >>= 2
            dst.putpixel((x//2,y//2),tuple(pd))
    return dst

# threshold RGB image into 2bpp greyscale palettized
def thresh4(src,t):
    dst = PIL.Image.new("P",src.size)
    for y in range(src.size[1]):
        for x in range(src.size[0]):
            p = src.getpixel((x,y))
            v = (p[0]+p[1]+p[2])//3
            pd = 0
            if v >= t[0]: pd = 1
            if v >= t[1]: pd = 2
            if v >= t[2]: pd = 3
            dst.putpixel((x,y),pd)
    return dst

# simple CHR + tilemap builder, no concept of palettes/flipping/priority, OR with given attribute
def tilemap(img,attribute,tw=8,th=8,planes=2):
    cd = bytearray() # CHR
    td = bytearray() # tilemap
    for ty in range(0,img.size[1],th):
        for tx in range(0,img.size[0],tw):
            c = chr(img,tx,ty,tw,th,planes)
            ci = -1
            for mi in range(0,len(cd),len(c)): # look for match in existing tiles
                if cd[mi:mi+len(c)] == c:
                    ci = mi // (16 * planes)
                    break
            if ci < 0: # add if not found
                ci = len(cd) // (16 * planes)
                cd.extend(c)
            td.append(ci & 255)
            td.append(attribute | (ci >> 8) & 255)
    return (cd,td)

#
# build
#

pal_ships1 = make_pal(rgb_img_load("ships1.png"))
pal_ships5 = make_pal(rgb_img_load("ships5.png"))
pal_sprite = make_pal(rgb_img_load("sprite.png"))
pal_water  = make_pal(rgb_img_load("water_pal.png"))

bin_save("ships1.pal",snes_pal(pal_ships1))
bin_save("ships5.pal",snes_pal(pal_ships5))
bin_save("sprite.pal",snes_pal(pal_sprite))
bin_save("water.pal",snes_pal(pal_water))

chr_sprite = chr(index_img_load("sprite.png",pal_sprite))
bin_save("sprite.chr",chr_sprite)

(chr_ships1,map_ships1) = tilemap(index_img_load("ships1.png",pal_ships1),1<<2,8,8)
(chr_ships5,map_ships5) = tilemap(index_img_load("ships5.png",pal_ships5),1<<2,16,8)
bin_save("ships1.chr",chr_ships1)
bin_save("ships5.chr",chr_ships5)
bin_save("ships1.nmt",map_ships1)
bin_save("ships5.nmt",map_ships5)

chr_water1 = bytearray()
chr_water5 = bytearray()
for i in range(16):
    t = (45,65,100)
    img5 = rgb_img_load("water\\caust_%03d.png" % (i+1))
    img5 = scalehalf(img5)
    img1 = scalehalf(img5)
    chr5 = chr(thresh4(img5,t),0,0,-1,-1,1)
    chr1 = chr(thresh4(img1,t),0,0,-1,-1,1)
    chr_water5.extend(chr5)
    chr_water1.extend(chr1) # duplicate 4x to be the same size as mode 5 version
    chr_water1.extend(chr1)
    chr_water1.extend(chr1)
    chr_water1.extend(chr1)
bin_save("water1.chr",chr_water1)
bin_save("water5.chr",chr_water5)
