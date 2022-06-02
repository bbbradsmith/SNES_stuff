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

# convert rectangles to CHR data, planes = 1 (2bpp) or 2 (4bpp) or 4 (8bpp)
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

#
# build
#

img  = rgb_img_load("elasticity.png") # hicolor
imgc = rgb_img_load("elasticity_8.png") # 8bpp no dither
imgd = rgb_img_load("elasticity_fs.png") # 8bpp floyd-steinberg

# a/b = flicker "blend"
imga = PIL.Image.new("RGB",img.size)
imgb = PIL.Image.new("RGB",img.size)
imgx = PIL.Image.new("RGB",img.size)
imgi = PIL.Image.new("RGB",(img.size[0],img.size[1]*2))
for y in range(img.size[1]):
    for x in range(img.size[0]):
        p = img.getpixel((x,y))
        r = p[0]
        g = p[1]
        b = p[2]
        # a = round down to 3:3:2
        ra = r & 0xE0
        ga = g & 0xE0
        ba = b & 0xC0
        # b = round up to 3:3:2
        rb = min(0xE0,(r + 0x10)) & 0xE0
        gb = min(0xE0,(g + 0x10)) & 0xE0
        bb = min(0xC0,(b + 0x20)) & 0xC0
        # expand brightness by duplicating bits
        ra |= (ra >> 3) | (ra >> 6)
        ga |= (ga >> 3) | (ga >> 6)
        ba |= (ba >> 2) | (ba >> 4) | (ba >> 6)
        rb |= (rb >> 3) | (rb >> 6)
        gb |= (gb >> 3) | (gb >> 6)
        bb |= (bb >> 2) | (bb >> 4) | (bb >> 6)
        # average version to compare against flickering target
        rx = (ra+rb)//2
        gx = (ga+gb)//2
        bx = (ba+bb)//2
        # imga = round up green, imgb = round up red+blue
        imga.putpixel((x,y),(ra,gb,ba))
        imgb.putpixel((x,y),(rb,ga,bb))
        imgx.putpixel((x,y),(rx,gx,bx))
        # test rounding
        #imga.putpixel((x,y),(ra,ga,ba))
        #imgb.putpixel((x,y),(rb,gb,bb))
        # interlaced version
        imgi.putpixel((x,(y*2)+0),imga.getpixel((x,y)))
        imgi.putpixel((x,(y*2)+1),imgb.getpixel((x,y)))

imga.save("flicker0.png")
imgb.save("flicker1.png")
imgx.save("flickerX.png")
imgi.save("flickerI.png")

# e/f = 3:3:2 base + 1:1:2 residual additive for 4:4:4 result
imge = PIL.Image.new("RGB",img.size)
imgf = PIL.Image.new("RGB",img.size)
imgr = PIL.Image.new("RGB",img.size)
for y in range(img.size[1]):
    for x in range(img.size[0]):
        p = img.getpixel((x,y))
        r = p[0]
        g = p[1]
        b = p[2]
        # e = round down to 3:3:2
        re = r & 0xE0
        ge = g & 0xE0
        be = b & 0xC0
        # f = residual of rounding down to 4:4:4
        rf = (r & 0xF0) - re
        gf = (g & 0xF0) - ge
        bf = (b & 0xF0) - be
        # test with brighter colours
        #rf *= 4
        #gf *= 4
        #bf *= 4        
        # expand brightness by duplicating bits
        re |= (re >> 4)
        ge |= (ge >> 4)
        be |= (be >> 4)
        rf |= (rf >> 4)
        gf |= (gf >> 4)
        bf |= (bf >> 4)
        # save
        imge.putpixel((x,y),(re,ge,be))
        imgf.putpixel((x,y),(rf,gf,bf))
        imgr.putpixel((x,y),(re+rf,ge+gf,be+bf))

imge.save("residual0.png")
imgf.save("residual1.png")
imgr.save("residualR.png")

imgs = (imga,imgb,imgc,imgd,imge,imgf)
imgsp = (4,4,4,4,4,2) # CHR planes
for i in range(len(imgs)):
    img = imgs[i]
    planes = imgsp[i]
    pal = make_pal(img)
    if i == 4: # prefix residual1 palette on residual0
        pal5 = make_pal(imgs[5])
        pal5.extend(pal)
        pal = pal5
    bin_save("%d.pal" % i, snes_pal(pal))
    # build CHR in 16x16 blocks
    imp = index_img(img,pal)
    row0 = bytearray()
    row1 = bytearray()
    bchr = bytearray()
    for y in range(0,img.size[1],16):
        for x in range(0,img.size[0],16):
            row0.extend(chr(imp,x,y+0,16,8,planes))
            row1.extend(chr(imp,x,y+8,16,8,planes))
            if len(row0) >= (16 * (16 * planes)): # whenever 16 tiles are reached, add both rows together
                bchr.extend(row0)
                bchr.extend(row1)
                row0 = bytearray()
                row1 = bytearray()
    if len(row0) > 0: # pad last row to 16
        pad = (16* (16 * planes))-len(row0)
        row0.extend([0] * pad)
        row1.extend([0] * pad)
        bchr.extend(row0)
        bchr.extend(row1)
    bin_save("%d.chr" % i, bchr)
