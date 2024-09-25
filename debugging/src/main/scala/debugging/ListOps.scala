package debugging

def contains(l: IntList, n: Int): Boolean =
  !l.isEmpty && (l.head == n || contains(l.tail, n))

def removeDuplicates(l: IntList): IntList =
  l match
    case IntNil() => IntNil()
    case IntCons(hd, tl) =>
      if contains(tl, hd) then removeDuplicates(tl)
      else IntCons(hd, removeDuplicates(tl))

def remove(l: IntList, n: Int): IntList =
  l match
    case IntNil()         => IntNil()
    case IntCons(`n`, tl) => remove(tl, n)
    case IntCons(m, tl)   => IntCons(m, remove(tl, n))

def removeDuplicates_alt(l: IntList): IntList =
  l match
    case IntNil() => IntNil()
    case IntCons(hd, tl) =>
      IntCons(hd, removeDuplicates_alt(remove(tl, hd)))

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
  val canCreateNewVipUser = createNewVipUser(id)
  contains(users, id) && canCreateNewVipUser
