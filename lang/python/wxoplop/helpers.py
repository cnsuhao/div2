def filecopy(inp, outp):
    with open(inp, 'rb') as fin, open(outp,'wb') as fout:
        fout.write( fin.read() )

def Color(_r=None, _g=None, _b=None, _a=None, **kw):
    R,G,B,A = range(4)
    have = [ v is not None for v in [_r, _g, _b, _a] ]
    
    # no arguments
    if not any(have) and len(kw.keys()) == 0:
        return wx.Color()
    
    # keyword arguments
    if len(kw.keys()) > 0:
        r = _r if have[R] else kw.get('r', 0)
        g = _g if have[G] else kw.get('g', 0)
        b = _b if have[B] else kw.get('b', 0)
        a = _a if have[A] else kw.get('a', 255)
    
    # otherwise we have arguments
    else:
        r = _r
        g = _g if have[G] and have[B] else r
        b = _b if have[G] and have[B] else r
        
        if all(have):
            a = _a
        elif have[G] and not have[B]:
            a = _g
        else:
            a = 255
        
    return wx.Color(r,g,b,a)