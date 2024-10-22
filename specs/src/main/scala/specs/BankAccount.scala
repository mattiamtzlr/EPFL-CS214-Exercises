package specs

enum AccOpResult:
  case Ok(oldBalance: Double)
  case InsufficientFund(oldBalance: Double)

  def old = this match
    case Ok(b)               => b
    case InsufficientFund(b) => b

protected class BankAccount(private var _balance: Double):
  import AccOpResult.*

  // gets reevaluated on each call, thus is always up-to-date with _balance
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
    require(amount >= 0)
    updateBalance(balance + amount)
  } ensuring(res => balance == res.old + amount)

  /** Withdraws the specified amount from the bank account.
    *
    * @param amount
    *   The amount to be withdrawn. Must be non-negative.
    */
  def withdraw(amount: Double): AccOpResult = {
    require(amount >= 0)
    if balance >= amount then
      updateBalance(balance - amount)
    else
      InsufficientFund(balance)
  } ensuring(
    res =>
      if balance >= amount then balance == res.old - amount
      else balance == res.old
  )

  /** Transfers the specified amount from this bank account to `that` bank
    * account.
    *
    * @param that
    *   The BankAccount to which the money should be transferred
    * @param amount
    *   The amount to be transferred. Must be non-negative.
    */
  def transfer(that: BankAccount, amount: Double): (AccOpResult, AccOpResult) = {
    require(amount >= 0)
    if this.balance >= amount then
      (this.withdraw(amount), that.deposit(amount))
    else
      (InsufficientFund(this.balance), Ok(that.balance))
  } ensuring(
    res =>
      if balance >= amount 
        then (this.balance, that.balance) == (res._1.old - amount, res._2.old + amount)
        else (this.balance, that.balance) == (res._1.old, res._2.old)
  )