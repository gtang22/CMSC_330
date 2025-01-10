# You should write all four classes in this file
class Person:
    def __init__(self, name, age):
        self.name = name
        self.age = age
    
    def get_age(self):
        return self.age
    
    def set_age(self, new_age):
        self.age = new_age
        return self
    

class Student(Person):
    def __init__(self, name, age, grade):
        super().__init__(name, age)
        self.grade = grade

    def get_grade(self):
        return self.grade
    
    def change_grade(self, new_grade):
        self.grade = new_grade
        return self
    
class Staff(Person):
    
    def __init__(self, name, age, position):
        super().__init__(name, age)
        self.position = position
    
    def get_position(self):
        return self.position
    
    def change_position(self, newPosition):
        self.position = newPosition 
        return self
    
class Roster:

    def __init__(self):
        self.array = []
        self.size_of_roster = 0
    
    def add(self, person):
        self.array.append(person)
        self.size_of_roster += 1
        return self
    
    def size(self):
        return self.size_of_roster
    
    def remove(self, person):
        self.size_of_roster -= 1
        self.array.remove(person)
        return self
    
    def get_person(self, name):
        for i in self.array:
            if name == i.name:
                return i
        return None
    
    def map(self, f):
        for i in self.array:
            f(i)
        return self

