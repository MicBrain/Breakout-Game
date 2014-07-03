#  Name: Rafayel Mkrtchyan
#  Email: rafamian@berkeley.edu

# Q0.
# Q1.

class VendingMachine(object):
    """A vending machine that vends some product for some price.

    >>> v = VendingMachine('candy', 10)
    >>> v.vend()
    'Machine is out of stock.'
    >>> v.restock(2)
    'Current candy stock: 2'
    >>> v.vend()
    'You must deposit $10 more.'
    >>> v.deposit(7)
    'Current balance: $7'
    >>> v.vend()
    'You must deposit $3 more.'
    >>> v.deposit(5)
    'Current balance: $12'
    >>> v.vend()
    'Here is your candy and $2 change.'
    >>> v.deposit(10)
    'Current balance: $10'
    >>> v.vend()
    'Here is your candy.'
    >>> v.deposit(15)
    'Machine is out of stock. Here is your $15.'
    """
    def __init__(self, chocolate_name, price):
        self.chocolate_name = chocolate_name
        self.price = price
        self.balance = 0
        self.stock = 0

    def vend(self):
        if self.stock == 0:
            return 'Machine is out of stock.'
        charge = self.price - self.balance
        if self.balance < self.price:
            return 'You must deposit ${0} more.'.format(charge)
        if charge != 0:
            self.balance = 0
            self.stock = self.stock - 1
            return 'Here is your {0} and ${1} change.'.format(self.chocolate_name, -charge) 
        self.balance = 0
        self.stock = self.stock - 1 
        return 'Here is your {0}.'.format(self.chocolate_name)
        
    def restock(self, money):
        assert type(money) is int and money>0, 'money should be positive and integer'
        self.stock = self.stock + money
        return 'Current {0} stock: {1}'.format(self.chocolate_name, self.stock)

    def deposit(self, amount):
        if self.stock == 0:
            return 'Machine is out of stock. Here is your ${0}.'.format(amount)
        self.balance = self.balance + amount
        return 'Current balance: ${0}'.format(self.balance)

# Q2.

class MissManners(object):
    """A container class that only forward messages that say please.

    >>> v = VendingMachine('teaspoon', 10)
    >>> v.restock(2)
    'Current teaspoon stock: 2'
    >>> m = MissManners(v)
    >>> m.ask('vend')
    'You must learn to say please first.'
    >>> m.ask('please vend')
    'You must deposit $10 more.'
    >>> m.ask('please deposit', 20)
    'Current balance: $20'
    >>> m.ask('now will you vend?')
    'You must learn to say please first.'
    >>> m.ask('please hand over a teaspoon')
    'Thanks for asking, but I know not how to hand over a teaspoon'
    >>> m.ask('please vend')
    'Here is your teaspoon and $10 change.'
    """
    def __init__(self,  variable):
        self.variable = variable

    def ask(self, message, *arg):
        magic = 'please '
        if magic not in message:
            return 'You must learn to say please first.'
        new = message[len(magic):]
        if not hasattr(self.variable, new):
            return 'Thanks for asking, but I know not how to ' + new
        return getattr(self.variable, new)(*arg)

# Q3.

from life import life

class life_lists(life):
    """An implementation of the Game of Life where the board is represented
    as a list of lists, one list per row.  The elements of the row lists
    are integers; odd integers represent cells with living organisms, and
    even integers represent empty cells."""

    def __init__(self, nrows, ncols, init=None):
        """A new Life board containing NROWS rows and NCOLS columns, which wrap around.
        If INIT is not None, then it should be a sequence (any iterable) of rows, each
        of which is itself a sequence (any iterable).   The values fill the board as
        for life.set_board."""
        super().__init__(nrows, ncols)
        self._board = [[0 for c in range(ncols)] for r in range(nrows)]
        if init is not None:
            self.set_board(init)

    def _is_alive(self, row, col):
        if self._board[row][col] % 2 == 1:
            return True
        else:
            return False

    def _set_alive(self, row, col, alivep):
        self._board[row][col] = alivep


    def tick(self):
        """Update the board to the next generation.
        >>> b = life_lists(10, 10,    # Glider
        ...                ("     ",
        ...                 "  *  ",
        ...                 "   *  ",
        ...                 " ***  ",
        ...                 "      "))
        >>> print(b, end="")
        ----------
        --*-------
        ---*------
        -***------
        ----------
        ----------
        ----------
        ----------
        ----------
        ----------
        >>> b.tick()
        >>> print(b, end="")
        ----------
        ----------
        -*-*------
        --**------
        --*-------
        ----------
        ----------
        ----------
        ----------
        ----------
        >>> b.tick()
        >>> b.tick()
        >>> b.tick()
        >>> print(b, end="")
        ----------
        ----------
        ---*------
        ----*-----
        --***-----
        ----------
        ----------
        ----------
        ----------
        ----------
        """
        
        arg0 = 0
        arg1 = copy.deepcopy(self._board)
        arg2 = len(arg1)

        while arg0 < arg2:
            count = 0
            arg0 = arg0 + 1

            while count < len(arg1[arg0]):
                count = count + 1

                if life.survives(self, arg0, count):
                    arg1[arg0][count] = 1
                else: 
                    arg1[arg0][count] = 0

        self._board = arg1


