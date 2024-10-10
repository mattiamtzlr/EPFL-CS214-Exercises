package specs

enum AccOpResult:
  case Ok(oldBalance: Double)
  case InsufficientFund(oldBalance: Double)

  def old = this match
    case Ok(b)               => b
    case InsufficientFund(b) => b

protected class BankAccount(private var _balance: Double):
  import AccOpResult.*

  def balance = _balance

  private def updateBalance(amount: Double): AccOpResult =
    val oldBalance = balance
    _balance = amount
    Ok(oldBalance)

  /** Deposits the specified amount into the bank account.
    *
    * @param amount
    *   The amount to be deposited. Must be non-negative.
    */
  def deposit(amount: Double): AccOpResult = {
    updateBalance(balance + amount)
  }

  /** Withdraws the specified amount from the bank account.
    *
    * @param amount
    *   The amount to be withdrawn. Must be non-negative.
    */
  def withdraw(amount: Double): AccOpResult = {
    if balance >= amount then
      updateBalance(balance - amount)
    else
      InsufficientFund(balance)
  }

  /** Transfers the specified amount from this bank account to `that` bank
    * account.
    *
    * @param amount
    *   The amount to be transferred. Must be non-negative.
    */
  def transfer(that: BankAccount, amount: Double): (AccOpResult, AccOpResult) = {
    if this.balance >= amount then
      (this.withdraw(amount), that.deposit(amount))
    else
      (InsufficientFund(this.balance), Ok(that.balance))
  }
