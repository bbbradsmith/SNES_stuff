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

def pal_pad16(palette,color=(0,0,0)):
    while len(palette) < 16:
        palette.append(color)
    return palette

def pal_dark(palette):
    return [(r//2,g//2,b//2) for (r,g,b) in palette]

def pal_invert(palette):
    return [(255-r,255-g,255-b) for (r,g,b) in palette]

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

def tilemap_merge(cda,tda,cdb,tdb,planes=2): # append 1 tilemap to another, adds CHR A count to second tilemap
    cd = bytearray(cda) + cdb
    td = bytearray(tda)
    ts = 16 * planes
    for i in range(0,len(tdb),2):
        t = (tdb[i+0] | ((tdb[i+1] & 3) << 8)) + (len(cda) // ts)
        td.append(t & 255)
        td.append((tdb[i+1] & 0xFC) | (t >> 8))
    return (cd,td)

#
# build
#

pal_bg1  = pal_pad16(make_pal(rgb_img_load("bg1.png"),  0,  0,256,256,[(0,0,0)]))
pal_bg2a = pal_pad16(make_pal(rgb_img_load("bg2.png"),  0,  0,256,144,[(0,0,0)]))
pal_bg2b = pal_pad16(make_pal(rgb_img_load("bg2.png"),  0,144,256,112,[(0,0,0)]))
pal_bg3  = pal_pad16(make_pal(rgb_img_load("bg3.png"),  0,  0,256,256,[(0,0,0)]))

pal_obj1 = pal_pad16(make_pal(rgb_img_load("obj.png"),  0,  0,128, 32,[(255,0,255)]))
pal_obj2 = pal_pad16(make_pal(rgb_img_load("obj.png"),  0, 32,128, 32,[(255,0,255)]))
pal_obj3 = pal_pad16(make_pal(rgb_img_load("obj.png"),  0, 64,128, 64,[(255,0,255)]))

pal_bg = pal_bg3 + pal_bg2a + pal_bg2b + pal_bg1
pal_bg += pal_invert(pal_bg)

pal_obd3 = pal_dark(pal_obj3)
pal_obd4 = pal_dark(pal_obd3)

pal_obj  = pal_obj1 + pal_invert(pal_obj1)
pal_obj += pal_obj2 + pal_invert(pal_obj2)
pal_obj += pal_obd3 + pal_invert(pal_obd3)
pal_obj += pal_obd4 + pal_invert(pal_obd4)

bin_save("pal.pal",snes_pal(pal_bg + pal_obj))

(chr_bg1, nmt_bg1 ) = tilemap(index_img_load("bg1.png",pal_bg1 ),3*4,8,8,2)
(chr_bg2a,nmt_bg2a) = tilemap(index_img_load("bg2.png",pal_bg2a).crop((  0,  0,256,144)),1*4,8,8,2)
(chr_bg2b,nmt_bg2b) = tilemap(index_img_load("bg2.png",pal_bg2b).crop((  0,144,256,256)),2*4,8,8,2)
(chr_bg3, nmt_bg3 ) = tilemap(index_img_load("bg3.png",pal_bg3 ),0*4|0x20,8,8,1)
(chr_bg2, nmt_bg2 ) = tilemap_merge(chr_bg2a,nmt_bg2a,chr_bg2b,nmt_bg2b)
bin_save("bg1.chr",chr_bg1)
bin_save("bg2.chr",chr_bg2)
bin_save("bg3.chr",chr_bg3)
bin_save("bg1.nmt",nmt_bg1)
bin_save("bg2.nmt",nmt_bg2)
bin_save("bg3.nmt",nmt_bg3)

chr_obj = bytearray()
chr_obj += chr(index_img_load("obj.png",pal_obj1),  0,  0,128, 32,2)
chr_obj += chr(index_img_load("obj.png",pal_obj2),  0, 32,128, 32,2)
chr_obj += chr(index_img_load("obj.png",pal_obj3),  0, 64,128, 64,2)
bin_save("obj.chr",chr_obj)