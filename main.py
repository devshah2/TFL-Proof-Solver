a = "(A & B)"
b = "((a|b)|c)"
c = "((a|b)>(c|d))"

rules={
    ["x&y"]:["x","y"],
    ["x<y"]: ["x>y","y<x"],
    ["x>y","x"]:["y"],
    ["",""]:[""]
}

def findLongest(x):
    opened=0
    for i, c in enumerate(x):
        # print("i={}, c={}, opened={}".format(i,c,opened))
        if(c=="("):
            opened+=1
        elif(c==")"):
            opened-=1
            if(opened==0):
                return i

def solve(x, goal=""):
    x=x.replace(" ","")
    x=x[1:-1]
    print(x)
    if(x[0]!="("):
        a=x[0]
        b=x[1]
        c=x[2:]
    else:
        i = findLongest(x)
        a=x[:i+1]
        b=x[i+1]
        c=x[i+2:]
    print(a)
    print(b)
    print(c)
    print()
    return [a,b,c]

        

solve(a)
solve(b)
solve(c)
