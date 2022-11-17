rules = None

def ask(pat):
    return [instantiate(pat, blst) for blst in prove(pat)]

def instantiate(pat, blst):
    if(var_p(pat)):
        bdg=var_binding(pat,blst)
        if(bdg):
            return instantiate(bdg[1],blst)
        return pat
    if(type(pat)!=type([])):
        return 


def var_binding(pat, blist):
    raise NotImplementedError

def var_p(pat):
    raise NotImplementedError

def prove(query, blists=[]):
    raise NotImplementedError