import PIL.Image
import random

# set image palette from RGB tuples
def index_setpal(img,palette): 
    linpal = [p for tup in palette for p in tup[0:3]]
    img.putpalette(linpal)

def lerp(a,b,t):
    return a + (b-a)*t
def blend(c0,c1,a=0.5):
    return (int(lerp(c0[0],c1[0],a)),int(lerp(c0[1],c1[1],a)),int(lerp(c0[2],c1[2],a)))

img = PIL.Image.new("P",(256,224),1)
WALLCOL = (0,0,0)
pal = [blend(WALLCOL,(255,255,255),0.25)] * 256 # background
pal[1  ] = WALLCOL # 1 = walls
pal[2  ] = pal[3  ] = (0,255,0) # 2-128 = good path
pal[129] = pal[130] = blend(pal[0],(128,0,255),0.75) # 129-255 = bad path
pal[4  ] = pal[128] = blend(pal[0],pal[2])
pal[131] = pal[255] = blend(pal[0],pal[129])
index_setpal(img,pal)

MW = 199
MH = 199
#MW = 9
#MH = 9
MSX = 0
MSY = MH-1
MEX = MW-1
MEY = 0
BRANCH_RATE = 10
maze = [1]*(MH*MW) # fill with walls
maze_flow = [None]*(MH*MW)
def maze_get(x,y):
    return maze[x+(MW*y)]
def maze_flow_get(x,y):
    return maze_flow[x+(MW*y)]
def maze_mark(x,y,v,px,py):
    maze[x+(MW*y)] = (v%127)+129
    maze_flow[x+(MW*y)] = (px,py)
def maze_good(x,y):
    maze[x+(MW*y)] = ((maze_get(x,y)-2)%127)+2

def maze_print():
    s = ""
    for y in range(MH):
        for x in range(MW):
            m = maze_get(x,y)
            if m == 1: s += "#"
            elif m >= 2 and m < 129:  s += "."
            else: s += " "
        s += "\n"
    print(s)

def maze_draw(img,scale=1):
    ox = (img.width  - (MW*scale)) // 2
    oy = (img.height - (MH*scale)) // 2
    for y in range(-1,MH+1):
        for x in range(-1,MW+1):
            c = 1
            if x >= 0 and x < MW and y >= 0 and y < MH:
                c = maze_get(x,y)
            for sy in range(scale):
                for sx in range(scale):
                    img.putpixel((ox+(x*scale)+sx,oy+(y*scale)+sy),c)

maze_stack = []
first_path = True

def maze_build(entry):
    (x,y,v) = entry
    #print("maze_build(%2d,%2d,%3d)" % (x,y,v))
    #maze_print()
    global maze_stack
    global first_path
    global MEX, MEY
    if x == MEX and y == MEY:
        first_path = False
        return
    o = [0,1,2,3]
    dx = [1,-1,0,0]
    dy = [0,0,-1,1]
    random.shuffle(o)
    for d in o:
        tx = x+(dx[d]*2) # destination cell
        ty = y+(dy[d]*2)
        wx = x+dx[d] # wall in between
        wy = y+dy[d]
        if tx < 0 or tx >= MW: continue # out of bounds
        if ty < 0 or ty >= MH: continue
        if maze_get(tx,ty) != 1: continue # already visited
        maze_mark(wx,wy,v+1,x,y)
        maze_mark(tx,ty,v+2,wx,wy)
        maze_stack.append((x,y,v)) # return to this node after
        maze_stack.append((tx,ty,v+2))
        return

# generate maze
maze_stack.append((MSX,MSY,0))
maze_mark(MSX,MSY,0,-1,-1)
while len(maze_stack) > 0:
    if first_path or (random.randint(0,99) > BRANCH_RATE):
        maze_build(maze_stack.pop()) # depth first
    else:
        maze_build(maze_stack.pop(random.randint(0,len(maze_stack)-1))) # random branch

# mark path from end to start
mx = MEX
my = MEY
flow = maze_flow_get(mx,my)
while flow:
    maze_good(mx,my)
    (mx,my) = flow
    if mx < 0 or my < 0: break
    flow = maze_flow_get(mx,my)
maze_print()

# draw it
maze_draw(img,1)
img.save("maze3.png")

for i in range(2,129):
    pal[i] = (255,0,0)
index_setpal(img,pal)
img.save("maze3s.png")
