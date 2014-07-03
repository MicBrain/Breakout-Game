#  Name: Rafayel Mkrtchyan
#  Email: rafamian@berkeley.edu

# Q1.

def reverse_list(s):
    """Reverse the contents of list s and return None.

    >>> digits = [6, 2, 9, 5, 1, 4, 1, 3]
    >>> reverse_list(digits)
    >>> digits
    [3, 1, 4, 1, 5, 9, 2, 6]
    >>> d = digits
    >>> reverse_list(d)
    >>> digits
    [6, 2, 9, 5, 1, 4, 1, 3]
    """
    midpoint = len(s) // 2
    length = len(s) - 1
    for i in range(0, midpoint):
        s[i], s[length - i] = s[length - i], s[i]

# Q2.

def card(n):
    """Return the playing card type for a positive n <= 13."""
    assert type(n) == int and n > 0 and n <= 13, "Bad card n"
    specials = {1: 'A', 11: 'J', 12: 'Q', 13: 'K'}
    return specials.get(n, str(n))

def shuffle(cards):
    """Return a shuffled list that interleaves the two halves of cards.

    >>> suits = ['â™¡', 'â™¢', 'â™¤', 'â™§']
    >>> cards = [card(n) + suit for n in range(1,14) for suit in suits]
    >>> cards[:12]
    ['Aâ™¡', 'Aâ™¢', 'Aâ™¤', 'Aâ™§', '2â™¡', '2â™¢', '2â™¤', '2â™§', '3â™¡', '3â™¢', '3â™¤', '3â™§']
    >>> cards[26:30]
    ['7â™¤', '7â™§', '8â™¡', '8â™¢']
    >>> shuffle(cards)[:12]
    ['Aâ™¡', '7â™¤', 'Aâ™¢', '7â™§', 'Aâ™¤', '8â™¡', 'Aâ™§', '8â™¢', '2â™¡', '8â™¤', '2â™¢', '8â™§']
    >>> shuffle(shuffle(cards))[:12]
    ['Aâ™¡', '4â™¢', '7â™¤', '10â™§', 'Aâ™¢', '4â™¤', '7â™§', 'Jâ™¡', 'Aâ™¤', '4â™§', '8â™¡', 'Jâ™¢']
    >>> cards[:12]  # Should not be changed
    ['Aâ™¡', 'Aâ™¢', 'Aâ™¤', 'Aâ™§', '2â™¡', '2â™¢', '2â™¤', '2â™§', '3â™¡', '3â™¢', '3â™¤', '3â™§']
    """
    new_list = []
    assert len(cards) % 2 == 0, 'len(cards) must be even'
    midpoint = len(cards) // 2
    for i in range (0, midpoint):
        new_list.append(cards[i])
        new_list.append(cards[i+half])
    return new_list

# Q3.

G = { 'A': ['B', 'D'], 'B': ['C'], 'C': ['F'], 'D': ['E'], 
      'E': ['F'], 'F': ['G'], 'G': ['A'] }


def is_circular(G):
    """Return true iff G represents a circular directed graph."""
    for v in G:
        if reaches_circularity(G, v):
            return True
    return False

def reaches_circularity(G, v0):
    """Returns true if there is a circularity in G in some path
    starting from vertex V0."""
    def is_path_to_cycle(v1):
        for w in G[v1]:
            if v0 == w:
                return True
            if is_path_to_cycle(w):
                return True
        return False
    return is_path_to_cycle(v0)


def reaches_circularity(G, v0):
    """Returns true if there is a circularity in G in some path
    starting from vertex V0.
    >>> G = { 'A': ['B', 'D'], 'B': ['C'], 'C': ['F'], 'D': ['E'], 
    ...       'E': ['F'], 'F': ['G'], 'G': ['A'] }
    >>> is_circular(G)
    True
    >>> G['F'] = []
    >>> is_circular(G)
    False
    """
    updates = []
    def new_function(v0):
        nonlocal updates
        for w in G[v0]:
            if w in updates:
                return True
            updates.append(w)
            if v0 == w:
                return True
            elif new_function(w):
                return True
        updates = []
        return False

    return new_function(v0)

# Q4.

def make_withdraw(balance):
    """Return a withdraw function with BALANCE as its starting balance.
    >>> withdraw = make_withdraw(1000)
    >>> withdraw(100)
    900
    >>> withdraw(100)
    800
    >>> withdraw(900)
    'Insufficient funds'
    """

    def withdraw(amount):
        nonlocal balance
        if amount > balance:
           return 'Insufficient funds'
        balance = balance - amount
        return balance
    return withdraw


def make_withdraw(balance, password):
    """Return a password-protected withdraw function.

    >>> w = make_withdraw(100, 'hax0r')
    >>> w(25, 'hax0r')
    75
    >>> w(90, 'hax0r')
    'Insufficient funds'
    >>> w(25, 'hwat')
    'Incorrect password'
    >>> w(25, 'hax0r')
    50
    >>> w(75, 'a')
    'Incorrect password'
    >>> w(10, 'hax0r')
    40
    >>> w(20, 'n00b')
    'Incorrect password'
    >>> w(10, 'hax0r')
    "Your account is locked. Attempts: ['hwat', 'a', 'n00b']"
    >>> w(10, 'l33t')
    "Your account is locked. Attempts: ['hwat', 'a', 'n00b']"
    """
    all_tries = []
    def withdraw(amount, attempt):
        nonlocal balance
        if len(all_tries) == 3:
            a = str(all_tries)
            return "Your account is locked." + a
        if attempt != password:
            all_tries.append(attempt)
            return "Incorrect password"
        if amount > balance:
            return "Insufficient funds"
        balance = balance -amount
        return balance
    return withdraw


# Q5.

def make_joint(withdraw, old_password, new_password):
    """Return a password-protected withdraw function that has joint access to
    the balance of withdraw.

    >>> w = make_withdraw(100, 'hax0r')
    >>> w(25, 'hax0r')
    75
    >>> make_joint(w, 'my', 'secret')
    'Incorrect password'
    >>> j = make_joint(w, 'hax0r', 'secret')
    >>> w(25, 'secret')
    'Incorrect password'
    >>> j(25, 'secret')
    50
    >>> j(25, 'hax0r')
    25
    >>> j(100, 'secret')
    'Insufficient funds'

    >>> j2 = make_joint(j, 'secret', 'code')
    >>> j2(5, 'code')
    20
    >>> j2(5, 'secret')
    15
    >>> j2(5, 'hax0r')
    10

    >>> j2(25, 'password')
    'Incorrect password'
    >>> j2(5, 'secret')
    "Your account is locked. Attempts: ['my', 'secret', 'password']"
    >>> j(5, 'secret')
    "Your account is locked. Attempts: ['my', 'secret', 'password']"
    >>> w(5, 'hax0r')
    "Your account is locked. Attempts: ['my', 'secret', 'password']"
    >>> make_joint(w, 'hax0r', 'hello')
    "Your account is locked. Attempts: ['my', 'secret', 'password']"
    """
    incorrect2 = withdraw(0, old_password)
    if type(incorrect2) == str:
        return 'Incorrect password'
    def new_function(amount, password):
        if password == new_password or password == old_password:
            return withdraw(amount, old_password)
        else:
            return withdraw(amount, password)
    return new_function




# Q6.

def triangle_area(a, b, h):
    """Connect a, b, and h so that a is the area of a triangle with base b and
    height h.

    >>> a, b, h = [connector(n) for n in ('area', 'base', 'height')]
    >>> triangle_area(a, b, h)
    >>> a['set_val']('user', 75.0)
    area = 75.0
    >>> b['set_val']('user', 15.0)
    base = 15.0
    height = 10.0
    """
    "*** YOUR CODE HERE ***"

# Q7.

def squarer(a, b):
    """The constraint that a*a=b.

    >>> x, y = connector('X'), connector('Y')
    >>> s = squarer(x, y)
    >>> x['set_val']('user', 10)
    X = 10
    Y = 100
    >>> x['forget']('user')
    X is forgotten
    Y is forgotten
    >>> y['set_val']('user', 16)
    Y = 16
    X = 4.0
    """
    "*** YOUR CODE HERE ***"


# Q8.

def pythagorean(a, b, c):
    """Connect a, b, and c into a network for the Pythagorean theorem:
    a*a + b*b = c*c

    >>> a, b, c = [connector(name) for name in ('A', 'B', 'C')]
    >>> pythagorean(a, b, c)
    >>> a['set_val']('user', 5)
    A = 5
    >>> c['set_val']('user', 13)
    C = 13
    B = 12.0
    """
    "*** YOUR CODE HERE ***"


def connector(name=None):
    """A connector between constraints.

    >>> celsius = connector('Celsius')
    >>> fahrenheit = connector('Fahrenheit')
    >>> converter(celsius, fahrenheit)

    >>> celsius['set_val']('user', 25)
    Celsius = 25
    Fahrenheit = 77.0

    >>> fahrenheit['set_val']('user', 212)
    Contradiction detected: 77.0 vs 212

    >>> celsius['forget']('user')
    Celsius is forgotten
    Fahrenheit is forgotten

    >>> fahrenheit['set_val']('user', 212)
    Fahrenheit = 212
    Celsius = 100.0
    """
    informant = None  # The source of the current val
    constraints = []  # A list of connected constraints

    def set_value(source, value):
        nonlocal informant
        val = connector['val']
        if val is None:
            informant, connector['val'] = source, value
            if name is not None:
                print(name, '=', value)
            inform_all_except(source, 'new_val', constraints)
        else:
            if val != value:
                print('Contradiction detected:', val, 'vs', value)

    def forget_value(source):
        nonlocal informant
        if informant == source:
            informant, connector['val'] = None, None
            if name is not None:
                print(name, 'is forgotten')
            inform_all_except(source, 'forget', constraints)
    connector = {'val': None,
                 'set_val': set_value,
                 'forget': forget_value,
                 'has_val': lambda: connector['val'] is not None,
                 'connect': lambda source: constraints.append(source)}

    return connector

def inform_all_except(source, message, constraints):
    """Inform all constraints of the message, except source."""
    for c in constraints:
        if c != source:
            c[message]()

def ternary_constraint(a, b, c, ab, ca, cb):
    """The constraint that ab(a,b)=c and ca(c,a)=b and cb(c,b)=a."""
    def new_value():
        av, bv, cv = [connector['has_val']() for connector in (a, b, c)]
        if av and bv:
            c['set_val'](constraint, ab(a['val'], b['val']))
        elif av and cv:
            b['set_val'](constraint, ca(c['val'], a['val']))
        elif bv and cv:
            a['set_val'](constraint, cb(c['val'], b['val']))
    def forget_value():
        for connector in (a, b, c):
            connector['forget'](constraint)
    constraint = {'new_val': new_value, 'forget': forget_value}
    for connector in (a, b, c):
        connector['connect'](constraint)
    return constraint

from operator import add, sub, mul, truediv

def adder(a, b, c):
    """The constraint that a + b = c."""
    return ternary_constraint(a, b, c, add, sub, sub)

def multiplier(a, b, c):
    """The constraint that a * b = c."""
    return ternary_constraint(a, b, c, mul, truediv, truediv)

def constant(connector, value):
    """The constraint that connector = value."""
    constraint = {}
    connector['set_val'](constraint, value)
    return constraint

def converter(c, f):
    """Connect c to f to convert from Celsius to Fahrenheit."""
    u, v, w, x, y = [connector() for _ in range(5)]
    multiplier(c, w, u)
    multiplier(v, x, u)
    adder(v, y, f)
    constant(w, 9)
    constant(x, 5)
    constant(y, 32)


