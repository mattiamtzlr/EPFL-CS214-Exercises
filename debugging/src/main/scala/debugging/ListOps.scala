package debugging

def contains(l: IntList, n: Int): Boolean =
  !l.isEmpty && (l.head == n || contains(l.tail, n))

def remove(l: IntList, n: Int): IntList =
  l match
    case IntNil()        => IntNil()
    case IntCons(h, t)   => if h == n then remove(t, n) else IntCons(h, remove(t, n))

def removeDuplicates(l: IntList): IntList =
  l match
    case IntNil() => IntNil()
    case IntCons(h, t) => IntCons(h, removeDuplicates(remove(t, h)))

var users: IntList = IntCons(101, IntCons(102, IntCons(103, IntNil())))
var vipUsers: IntList = IntCons(102, IntNil())

def deleteAllUsers() =
  users = IntNil()
  vipUsers = IntNil()

def getUsers(): IntList = users
def getVipUsers(): IntList = vipUsers

def createNewUser(id: Int): Boolean =
  if contains(users, id) then false
  else
    users = IntCons(id, users)
    true

def createNewVipUser(id: Int): Boolean =
  if contains(vipUsers, id) then false
  else
    vipUsers = IntCons(id, vipUsers)
    true

def upgradeUserToVip(id: Int): Boolean =
  contains(users, id) && createNewVipUser(id)