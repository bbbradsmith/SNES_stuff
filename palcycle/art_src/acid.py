import PIL.Image
import math
import random

# set image palette from RGB tuples
def index_setpal(img,palette): 
    linpal = [p for tup in palette for p in tup[0:3]]
    img.putpalette(linpal)

# get RGB tuples from image palette
def index_getpal(img):
    linpal = img.getpalette()
    pal = []
    for i in range(0,len(linpal)-2,3):
        pal.append(tuple(linpal[i:i+3]))
    return pal

# linear palette ramp in fractions of 32
P0 = 0 # all 0
P1 = 1 # all 31
PU = 2 # 0-31
PD = 3 # 31-0
def palrange(vals,step=1): # vals should be RGB triple (P0,P1,PD)
    p = []
    for i in range(0,32,step):
        c = []
        for v in vals:
            if v == P0:
                c.append(0)
            elif v == P1:
                c.append(31)
            elif v == PU:
                c.append(i)
            elif v == PD:
                c.append(31-i)
        p.append(tuple(c))
    return p

# hue cycle
def il(a,b,t):
    return (a*(1-t))+(b*t)
def hue(x):
    hf = (x * 3) / 32
    hr = math.floor(hf)
    ht = hf-hr
    hr %= 3
    c = (0,0,0)
    if hr == 0:
        c = (il(1,0,ht),il(0,1,ht),0)
    elif hr == 1:
        c = (0,il(1,0,ht),il(0,1,ht))
    else:
        c = (il(0,1,ht),0,il(1,0,ht))
    cs = 31 / max(c)
    return tuple(int(y*cs) for y in c)
def palhue(start,step=1):
    return [hue(start+i) for i in range(0,32,step)]    

def palexpand(p):
    return [(int((r*255)/31),int((g*255)/31),int((b*255)/31)) for (r,g,b) in p]

# some plasma ideas loosely based on acidwarp
# http://www.noah.org/acidwarp/
def dist(x,y):
    return math.sqrt((x*x)+(y*y))
def plasma(img,cmin,cmax,xcen,ycen,func,thresh=256):
    for y in range(0,img.height):
        for x in range(0,img.width):
            dx = x - xcen
            dy = y - ycen
            d = dist(dx,dy)
            angle = math.atan2(dy,dx)
            c = 0
            if func == 0: # waves
                SS = 0.01
                SW = 3
                c = (angle / math.tau) + \
                    math.sin(d * SS) + \
                    math.cos(x * SW / img.width) + \
                    math.cos(y * SW / img.height)
            elif func == 1: # peacock
                PD = img.width / 12
                SP = 0.15
                SS = 0.1
                c = (angle / math.tau) + \
                    math.sin(dist(dx   ,dy-PD) * SP) * SS + \
                    math.sin(dist(dx+PD,dy+PD) * SP) * SS + \
                    math.sin(dist(dx-PD,dy+PD) * SP) * SS
            elif func == 2: # interference quilt
                SW = 4
                SC = 0.3
                c = math.cos( 7 * x * SW / img.width ) * SC + \
                    math.cos( 7 * y * SW / img.height) * SC + \
                    math.cos(11 * x * SW / img.width ) * SC + \
                    math.cos(11 * y * SW / img.height) * SC
            elif func == 3: # rain
                RS = 0.2
                ca = random.random()
                cb = random.random()
                if x>0:
                    ca = (img.getpixel((x-1,y)) - cmin) / (cmax-cmin)
                if y>0:
                    cb = (img.getpixel((x,y-1)) - cmin) / (cmax-cmin)
                c = ((ca + cb) / 2) + ((random.random() - 0.5) * RS)
            c = c - math.floor(c)
            c = int(c * (cmax-cmin)) % (cmax-cmin)
            c += cmin
            if (c < thresh):
                img.putpixel((x,y),c)

img = PIL.Image.new("P",(256,224))
p = []
p += palrange((PU,PU,PU))
p += palrange((P1,P1,PD))
p += palrange((PD,P1,P0))
p += palrange((P0,PD,P0))
p += palrange((PU,P0,PU))
p += palrange((P1,PU,P1))
p += palrange((PD,PD,PD))
p += palhue(0,1)
p = palexpand(p)
index_setpal(img,p)
plasma(img,240,256,128,224//2,2)
plasma(img,64,128,128,224//2,0,96)
img.save("acid1.png")

overlay = PIL.Image.open("overlay.png")
assert(overlay.mode == "P")
p = [(0,0,0)] * 256
p[0x10:0x30] = palrange((PU,PU,PU))
p[0x30:0x50] = palrange((PD,PD,P1))
p[0x50:0x70] = palrange((PU,PU,P1))
p[0x70:0x90] = palrange((PD,PD,PD))
p[0x90:0xB0] = palrange((PD,PU,PD))
p[0xB0:0xD0] = palrange((PD,P1,PD))
p[0xD0:0xF0] = palrange((PU,PD,PU))
p = palexpand(p)
po = index_getpal(overlay)
p[0x00:0x10 ] = po[0x00:0x10 ]
p[0xF0:0x100] = po[0xF0:0x100]
index_setpal(img,p)
plasma(img,0x10,0x4F,0,0,3)
img = img.transpose(PIL.Image.FLIP_TOP_BOTTOM)
plasma(img,0x90,0xD0,90,60,1,0xB0)
img = img.transpose(PIL.Image.FLIP_TOP_BOTTOM)
for y in range(img.height):
    for x in range(img.width):
        p = overlay.getpixel((x,y))
        if p: img.putpixel((x,y),p)
img.save("acid2.png")
