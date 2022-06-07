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

# convert RGB image to indexed palette
def index_img(img,palette):
    src = img
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

# load and convert image to indexed palette
def index_img_load(filename, palette):
    src = PIL.Image.open(filename).convert("RGB")
    return index_img(src, palette)

# extract colours from an image to make a palette (row order)
def make_pal(img,x=0,y=0,w=-1,h=-1,palette=None):
    img = img.convert("RGB")
    if palette == None:
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

# convert rectangles to CHR data, chunky 8bpp
def ch7(img,x=0,y=0,w=-1,h=-1):
    b = bytearray()
    if w < 0: w = img.size[0] - x
    if h < 0: h = img.size[1] - y
    for ty in range(y,y+h,8):
        for tx in range(x,x+w,8):
            for py in range(8):
                for px in range(8):
                    p = img.getpixel((tx+px,ty+py)) & 0xFF
                    b.append(p)
    return b

# convert rectangles to CHR data, planes = 1 (2bpp) or 2 (4bpp) or 4 (8bpp) or 7 (chunky 8bpp)
def chr(img,x=0,y=0,w=-1,h=-1,planes=2):
    if planes == 7:
        return ch7(img,x,y,w,h)
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
    ts = 16 * planes
    if planes == 7: # mode 7
        ts = 64
    for ty in range(0,img.size[1],th):
        for tx in range(0,img.size[0],tw):
            c = chr(img,tx,ty,tw,th,planes)
            ci = -1
            for mi in range(0,len(cd),len(c)): # look for match in existing tiles
                if cd[mi:mi+len(c)] == c:
                    ci = mi // ts
                    break
            if ci < 0: # add if not found
                ci = len(cd) // ts
                cd.extend(c)
            td.append(ci & 255)
            td.append(attribute | (ci >> 8) & 255)
    if planes == 7: # mode 7 replaces attribute bytes with interleaved CHR
        for i in range(len(cd)):
            tdi = (i * 2) + 1
            while len(td) <= tdi:
                tdi.append(0) # make sure space for data exists
            td[tdi] = cd[i]
        return td
    return (cd,td)

#
# build
#

# gather palettes for both layers
pal_bg0 = [(255,0,255)] # start with magenta
pal_bg0 = make_pal(rgb_img_load("layer0.png"),0,0,-1,-1,pal_bg0)
pal_bg1 = [(255,0,255)]
pal_bg1 = make_pal(rgb_img_load("layer1.png"),0,0,-1,-1,pal_bg0)
# merge palettes: layer0 = first 128, layer 1 = last 128
pal_bg = pal_bg0 + [(64,64,64)]*(128-len(pal_bg0)) + pal_bg1

# darken bg1 colors because EXTBG doesn't use them, to indicate use
for i in range(128,len(pal_bg)):
    p = pal_bg[i]
    pd = tuple([int(x*0.6) for x in p])
    pal_bg[i] = pd

# gather palettes for sprites
pal_fg = [(255,0,255)]
pal_fg = make_pal(rgb_img_load("sprite.png"),0,0,-1,-1,pal_fg)
# merge palettes: last 16 for sprites
pal = pal_bg + [(64,64,64)]*((256-16)-len(pal_bg)) + pal_fg

# fill to 256 colours
pal = pal + [(64,64,64)]*(256-len(pal))
pal[0] = (0,0,0) # black background
bin_save("pal.pal",snes_pal(pal))

# build sprites
chr_sprite = chr(index_img_load("sprite.png",pal_fg))
bin_save("sprite.chr",chr_sprite)

# index the EXTBG layers and combine
img0 = index_img_load("layer0.png",pal_bg0)
img1 = index_img_load("layer1.png",pal_bg1)
img = img0
for y in range(img1.size[1]):
    for x in range(img1.size[1]):
        i1 = img1.getpixel((x,y))
        io = i1 + 128 # layer1 goes to last 128 colours
        if i1 == 0: # transparency replaced by lower level
            io = img0.getpixel((x,y))
        img.putpixel((x,y),io) # replaced img0

# build mode 7 indexed map 
md7_index = tilemap(img,0,8,8,7)
bin_save("index.md7",md7_index)

# convert to direct color BBGGGRRR
md7_direct = md7_index[:]
for i in range(1,len(md7_direct),2):
    pi = md7_direct[i]
    pc = pal[pi]
    dc = \
        (pc[2] & 0xC0) | \
        ((pc[1]>>2) & 0x38) | \
        ((pc[0]>>5) & 0x07)
    md7_direct[i] = dc
bin_save("direct.md7",md7_direct)
