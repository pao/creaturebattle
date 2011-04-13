import hashlib as H
import sys

salt = '.NaCl.$^d43lwz;)3s.optimize.this'

def mangle(monsters):
    cleaned = [s.lower().strip() for s in monsters]
    return [''.join([s, rs, salt]) for (s, rs) in zip(cleaned, reversed(cleaned))]

def main(monsters):
    if monsters[0] == monsters[1]:
        print 'The %s refuses to fight itself!'%(monsters[0],)
    else:
        mangled_monsters = [H.sha1(m).digest() for m in mangle(monsters)]
        battle = dict(zip(mangled_monsters, monsters))
        print 'The %s has won the battle!'%(battle[sorted(battle.keys())[0]],)

if __name__ == '__main__':
    main(sys.argv[1:3])
