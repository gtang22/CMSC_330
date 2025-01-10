from src.basics import *
from src.roster import *
from src.hof import *


# basics.py tests (Part 1 + Part 2)
def test_isPalindrome():
    assert(True == isPalindrome(0))
    assert(True == isPalindrome(1))
    assert(False == isPalindrome(10))
    assert(True == isPalindrome(101))
    assert(False == isPalindrome(120210))

def test_nthmax():
    assert(3 == nthmax(0, [1,2,3,0]))
    assert(2 == nthmax(1, [3,2,1,0]))
    assert(4 == nthmax(2, [7,3,4,5]))
    assert(nthmax(5, [1,2,3]) == None)

def test_freq():
    assert("" == freq(""))
    assert("a" == freq("aaabb"))
    assert("a" == freq("bbaaa"))
    assert("s" == freq("ssabcd"))
    assert("x" == freq("a12xxxxxyyyxyxyxy"))

def test_zipHash():
    assert({} == zipHash([], []))
    assert({1:2} == zipHash([1], [2]))
    assert({1:2, 5:4} == zipHash([1, 5], [2, 4]))
    assert(zipHash([1], [2,3]) == None)
    assert({"Mamat":"prof", "Hicks":"prof", "Vinnie":"TA"} ==
                  zipHash(["Mamat", "Hicks", "Vinnie"], ["prof", "prof", "TA"]))

def test_hashToArray():
    assert([] == hashToArray({}))
    assert([["a", "b"]] == hashToArray({"a":"b"}))
    assert([["a", "b"], [1, 2]] == hashToArray({"a":"b", 1:2}))
    assert([["x", "v"], ["y", "w"], ["z", "u"]] == hashToArray({"x":"v", "y":"w", "z":"u"}))
  
def test_public_maxlambda():
  assert(8 == maxLambdaChain(2,[(lambda x : x + 6)]))
  assert(24 == maxLambdaChain(2,[(lambda x : x + 4), (lambda x : x * 4)]))
  assert(-1 == maxLambdaChain(-4,[(lambda x : x * 4), (lambda x : x + 3)]))

# roster.py tests (Part 3)
def test_age():
    age = 42
    person = Person("person", age)
    assert(age == person.get_age())

    new_age = 84
    person.set_age(new_age)
    assert(new_age == person.get_age())

def test_grade():
    grade = 42.0
    student = Student("student", 0, grade)
    assert(grade == student.get_grade())
    
    new_grade = 84.0
    student.change_grade(new_grade) 
    assert(new_grade == student.get_grade())

def test_position():
    pos = "TA"
    staff = Staff("staff", 0, pos)
    assert(pos == staff.get_position())

    new_pos = "Professor"
    staff.change_position(new_pos)
    assert(new_pos == staff.get_position())

def test_public_roster():
    roster = Roster()
    assert(0 == roster.size())

    person = Person("person", 21)
    roster.add(person)
    assert(1 == roster.size())

    roster.map(lambda x: x.set_age(0))
    assert(person == roster.get_person("person"))
    assert(0 == roster.get_person("person").get_age())

# hof.py tests (Part 4)
def test_uniq():
  a = [1,7,7,1,5,2,7,7]
  b = [True,False,False,True,False,False,False]
  c = []

  assert sorted([1,2,5,7]) == sorted(uniq(a)), "uniq 1"
  assert sorted([False,True]) == sorted(uniq(b)), "uniq 2"
  assert [] == uniq(c), "uniq 3"

def test_find_max():
  a = [[1,2,3,4,5] for i in range(5)]
  assert 5 == find_max(a), "find_max 1"

  a[2][2] = 10
  assert 10 == find_max(a), "find_max 2"

def test_count_ones():
  a = [[1,1,1,1,1] for i in range(3)]
  b = [[1,1,1,1,1] if i%2==0 else [0,0,0,0,0] for i in range(10)]
  
  assert 15 == count_ones(a), "count_ones 1"
  assert 25 == count_ones(b), "count_ones 2"

def test_addgen():
  f1 = addgenerator(5)
  f2 =  addgenerator(10)

  assert 8 == f1(3), "addgen 1"
  assert 23 == f1(3) + f2(5), "addgen 1"

def test_apply_to_self():

  assert 12 == apply_to_self()(3,lambda a: 3**2),"appy to self 1"

def test_map2():
  m = [[1,2,3],[4,5,6],[7,8,9]]
  f = lambda x: x * 2
  assert [[2,4,6],[8,10,12],[14,16,18]] == map2(m,f), "map2 1"

  m2 = [[1,0,1],[1,0,1]]
  assert [[True,False,True],[True,False,True]] == map2(m2,lambda x: True if x == 1 else 0), "map2 2"