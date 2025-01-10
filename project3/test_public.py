from src.fsm import char,concat,union,star,nfa_to_dfa,move,e_closure, accept
from src.fsm import Fsm
from test.visualizer import make_visual


def test_concat():
  rege = "abc"
  a = concat(char("a"),concat(char("b"),char("c")))
  assert(accept(a,"abc"))
  assert(not(accept(a,"a")))
  assert(not(accept(a,"b")))
  assert(not(accept(a,"c")))
  assert(not(accept(a,"ab")))
  assert(not(accept(a,"bc")))
  assert(not(accept(a,"ac")))
  assert(not(accept(a,"cab")))
  assert(not(accept(a,"cba")))

def test_or1():
  rege = "b|c"
  a = union(char("b"),char("c"))
  assert(accept(a,"b"))
  assert(accept(a,"c"))
  assert(not(accept(a,"bc")))
  assert(not(accept(a,"cb")))
  assert(not(accept(a,"bb")))
  assert(not(accept(a,"cc")))

def test_or2():
  rege = "a|bc"
  f = union(char("a"),concat(char("b"),char("c")))
  assert(accept(f,"bc"))
  assert(accept(f,"a"))
  assert(not(accept(f,"ac")))
  assert(not(accept(f,"ab")))
  assert(not(accept(f,"ba")))

def test_move1():
  a = Fsm(['a','b'],[1,2,3,4,5,6,7],1,[3],[(6,'b',7),(2,'b',5),(1,'a',4),(1,'b',2),(1,'b',3)])
  m = (move('b',[1,6],a))
  assert(set(m) == set([7,2,3]))
  assert(set(e_closure(m,a)) == set([7,2,3]))

def test_move2():
  trans = [(1,'b',6),
           (1,'c',2),
           (1,'a',5),
           (2,'b',5),
           (4,'a',5),
           (4,'b',3),
           (4,'epsilon',5),
           (5,'a',3),
           (5,'epsilon',6),
           (6,'a',4),
           (6,'a',3),
           (6,'epsilon',4)
          ]
  b = Fsm(['a','b','c'],[1,2,3,4,5,6],1,[3],trans)
  assert(move('a',[1],b) == [5])
  assert(set(e_closure([5],b))==set([4,5,6]))

def test_ntd():
  trans = [(1,'b',6),
           (1,'c',2),
           (1,'a',5),
           (2,'b',5),
           (4,'a',5),
           (4,'b',3),
           (4,'epsilon',5),
           (5,'a',3),
           (5,'epsilon',6),
           (6,'a',4),
           (6,'a',3),
           (6,'epsilon',4)
          ]
  b = Fsm(['a','b','c'],[1,2,3,4,5,6],1,[3],trans)
  assert(accept(b,"ba"))
  assert(accept(b,"aa"))

def test_nfa_accept():
  m1 = Fsm(['a','b'],[0,1],0,[1],[(0,'a',1)])
  make_visual(m1)
  assert(not(accept(m1,"")))
  assert(not(accept(m1,"b")))
  assert(not(accept(m1,"ba")))
  assert(accept(m1,"a"))

  m2 = Fsm(['a','b'],[0,1,2],0,[2],[(0,'a', 1),(0,'b', 2)])
  assert(not(accept(m2,"")))
  assert(not(accept(m2,"a")))
  assert(accept(m2,"b"))
  assert(not(accept(m2,"ab")))
  assert(not(accept(m2,"ba")))

def test_nfa_to_dfa():
  m1 = Fsm(['a','b'],[0, 1, 2, 3],0,[1,3],[(0,'a', 1), (0,'a', 2), (2,'b', 3)])
  m2 = nfa_to_dfa(m1)
  assert(not(accept(m2,"")))
  assert(accept(m2,"a"))
  assert(accept(m2,"ab"))
  assert(not(accept(m2,"b")))
  assert(not(accept(m2,"ba")))
  
  m3 = Fsm(['a','b'],[0, 1, 2],0,[2],[(0,'a',1),(0,'b',2)])
  m4 = nfa_to_dfa(m3) 
  assert(not(accept(m4,"")))
  assert(not(accept(m4,"a")))
  assert(accept(m4,"b")) 
  assert(not(accept(m4,"ba")))

def test_nfa_closure():
  m1 = Fsm(['a'],[0,1],0,[1],[(0,'a',1)])
  assert(e_closure([0],m1) == [0])
  assert(e_closure([1],m1) == [1])

  m2 = Fsm([],[0,1],0,[1],[(0,"epsilon",1)])
  assert(set(e_closure([0],m2)) == set([0,1]))
  assert(e_closure([1],m2) == [1])
  m3 = Fsm(['a','b'],[0,1,2],0,[2],[(0,'a',1),(0,'b',2)])
  assert(e_closure([0],m3) == [0])
  assert(e_closure([1],m3) == [1])
  assert(e_closure([2],m3) == [2])
  m4 = Fsm(['a'],[0,1,2],0,[2],[(0,'epsilon',1),(0,'epsilon',2)])
  assert(set(e_closure([0],m4)) == set([0,1,2]))
  assert(e_closure([1],m4) == [1])
  assert(e_closure([2],m4) == [2])

def test_nfa_move():
  m1 = Fsm(['a'],[0,1],0,[1],[(0,'a',1)])
  assert(move('a',[0],m1) ==  [1])
  assert(move('a',[1],m1) ==  [])

  m2 = Fsm(['a'],[0,1],0,[1],[(0,'epsilon',1)])
  assert(move('a',[0],m2) ==  [])
  assert(move('a',[1],m2) ==  [])

  m3 = Fsm(['a','b'],[0,1,2],0,[2],[(0,'a',1),(0,'b',2)])
  assert(move('a',[0],m3) ==  [1])
  assert(move('a',[1],m3) ==  [])
  assert(move('a',[2],m3) ==  [])
  assert(move('b',[0],m3) ==  [2])
  assert(move('b',[1],m3) ==  [])
  assert(move('b',[2],m3) ==  [])

  m4 = Fsm(['a','b'],[0,1,2],0,[2],[(0,'epsilon',1),(0,'a',2)])
  assert(move('a',[0],m4) ==  [2])
  assert(move('a',[1],m4) ==  [])
  assert(move('a',[2],m4) ==  [])
  assert(move('b',[0],m4) ==  [])
  assert(move('b',[1],m4) ==  [])
  assert(move('b',[2],m4) ==  [])

def test_re_to_nfa():
  m1 = char("a") 
  assert(not(accept(m1,"")))
  assert(accept(m1,"a"))
  assert(not(accept(m1,"b")))
  assert(not(accept(m1,"ba")))

  m2 = union(char("a"),char("b"))
  assert(not(accept(m2,"")))
  assert(accept(m2,"a"))
  assert(accept(m2,"b"))
  assert(not(accept(m2,"ba")))
