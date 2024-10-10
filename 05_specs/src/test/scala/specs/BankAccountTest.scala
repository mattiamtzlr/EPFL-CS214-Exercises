package specs

class BankAccountTest extends munit.FunSuite:

  test("deposit 50 to account with balance 100"):
    val account = new BankAccount(100.0)
    account.deposit(50.0)
    assertEquals(account.balance, 150.0)

  test("withdraw 50 from account with balance 100"):
    val account = new BankAccount(100.0)
    account.withdraw(50.0)
    assertEquals(account.balance, 50.0)

  test("withdraw 50 from account with balance 10"):
    val account = new BankAccount(10.0)
    account.withdraw(50.0)
    assertEquals(account.balance, 10.0)

  test("transfer 50 from account with balance 100"):
    val account0 = new BankAccount(100.0)
    val account1 = new BankAccount(10.0)
    account0.transfer(account1, 50.0)
    assertEquals(account0.balance, 50.0)
    assertEquals(account1.balance, 60.0)

  test("transfer 50 from account with balance 10"):
    val account0 = new BankAccount(10.0)
    val account1 = new BankAccount(100.0)
    account0.transfer(account1, 50.0)
    assertEquals(account0.balance, 10.0)
    assertEquals(account1.balance, 100.0)

