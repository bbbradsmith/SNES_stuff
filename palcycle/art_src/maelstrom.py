import PIL.Image
import math

def index_setpal(img,palette): 
    linpal = [p for tup in palette for p in tup[0:3]]
    img.putpalette(linpal)

def lerp(a,b,t):
    return a + (b-a)*t
def blend(c0,c1,a=0.5):
    return (int(lerp(c0[0],c1[0],a)),int(lerp(c0[1],c1[1],a)),int(lerp(c0[2],c1[2],a)))

img = PIL.Image.new("P",(256,224),1)
pal = []
for y in range(16):
    for x in range(16):
        pal.append((
            int((x*255)/15),
            int(lerp(x,y,0.5)*255/15),
            int((y*255)/15)))
index_setpal(img,pal)

# "lines of equipotential"
# loosely based on the descrption of Maelstrom:
# http://eyecandyarchive.com/Maelstrom%20(MS-DOS)/

def potential(x,y):
     m = (x*x) + (y*y)
     return 0 if m == 0 else math.log(1 / m)

def maelstrom(img,charges,lines,scale=1,orient=0):
    for y in range(img.height):
        for x in range(img.width):
            a = 0
            p = 0
            for (cx,cy,cp) in charges:
                mag = (scale*cp/100) * potential(x-cx,y-cy)
                ang = math.atan2(x-cx,y-cy) * lines / math.tau
                ang -= math.floor(ang)
                p += mag
                a += ang
                p -= math.floor(p)
                a -= math.floor(a)
            ia = int(a * 16) % 16
            ip = int(p * 16) % 16
            if orient == 0:
                img.putpixel((x,y),(ia*16)+ip) # angle : potential
            else:
                img.putpixel((x,y),((15-ip)*16)+ia) # potential : angle

charges1 = [(135,161,20),
           (127,142,20),
           (120,10,40),
           (221,35,76),
           (259,211,50),
           (30,186,80)]
maelstrom(img,charges1,10)
img.save("maelstrom1.png")

charges2 = [(-10,-145,400),
            (240,600,300),
            (150,-30,600),
            (-300,160,300),
            ]
maelstrom(img,charges2,31,0.5,1)
img.save("maelstrom2_flow.png")

# use 16x16 image as a "palette" image
spots = PIL.Image.open("lace.png")
pal = []
for y in range(0,16):
    for x in range(0,16):
        pal.append(spots.getpixel((x,y)))
index_setpal(img,pal)

def preview(img,ox,oy,os): 
    for y in range(0,16):
        for x in range(0,16):
            for sx in range(os):
                for sy in range(os):
                    img.putpixel((ox+(x*os)+sx,oy+(y*os)+sy),x+(y*16))

preview(img,256-(16*5),224-(16*5),2)
preview(img,256-(16*3),224-(16*5),2)
preview(img,256-(16*5),224-(16*3),2)
preview(img,256-(16*3),224-(16*3),2)

img.save("maelstrom2.png")
