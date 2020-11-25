




def computeLCMFor2Numbers(a,b):
    return a*b/euclidean(a,b)

def computeLCMForAList(l,result):
    if len(l)==1:
        return computeLCMFor2Numbers(l[0],result)
    else:
        return computeLCMForAList(l[0:],result)

def euclidean(a,b):
    if b>0:
        return euclidean(b,a%b)
    else:
        return a


def main():
    pass
    
main()