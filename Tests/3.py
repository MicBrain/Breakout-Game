#  Name: Rafayel Mkrtchyan  
#  Email: rafamian@berkeley.edu


def str_interval(x):
    """Return a string representation of interval x.

    >>> str_interval(interval(-1, 2))
    '-1 to 2'
    """
    return '{0} to {1}'.format(lower_bound(x), upper_bound(x))

def add_interval(x, y):
    """Return an interval that contains the sum of any value in interval x and
    any value in interval y.

    >>> str_interval(add_interval(interval(-1, 2), interval(4, 8)))
    '3 to 10'
    """
    lower = lower_bound(x) + lower_bound(y)
    upper = upper_bound(x) + upper_bound(y)
    return interval(lower, upper)

def mul_interval(x, y):
    """Return the interval that contains the product of any value in x and any
    value in y.

    >>> str_interval(mul_interval(interval(-1, 2), interval(4, 8)))
    '-8 to 16'
    """
    p1 = lower_bound(x) * lower_bound(y)
    p2 = lower_bound(x) * upper_bound(y)
    p3 = upper_bound(x) * lower_bound(y)
    p4 = upper_bound(x) * upper_bound(y)
    return interval(min(p1, p2, p3, p4), max(p1, p2, p3, p4))


# Q1.

def interval(a, b):
    """Construct an interval from a to b."""
    return (a, b)

def lower_bound(x):
    """Return the lower bound of interval x."""
    return x[0]

def upper_bound(x):
    """Return the upper bound of interval x."""
    return x[1]

# Q2.

def div_interval(x, y):
    """Return the interval that contains the quotient of any value in x divided
    by any value in y.

    Division is implemented as the multiplication of x by the reciprocal of y.

    >>> str_interval(div_interval(interval(-1, 2), interval(4, 8)))
    '-0.25 to 0.5'
    """
    assert (lower_bound(y)!= 0 and upper_bound(y)!=0), "Can't devide by 0"
    reciprocal_y = interval(1/upper_bound(y), 1/lower_bound(y))
    return mul_interval(x, reciprocal_y)

# Q3.

def sub_interval(x, y):
    """Return the interval that contains the difference between any value in x
    and any value in y.

    >>> str_interval(sub_interval(interval(-1, 2), interval(4, 8)))
    '-9 to -2'
    """
    p1 = lower_bound(x) - lower_bound(y)
    p2 = lower_bound(x) - upper_bound(y)
    p3 = upper_bound(x) - lower_bound(y)
    p4 = upper_bound(x) - upper_bound(y)
    return interval(min(p1, p2, p3, p4), max(p1, p2, p3, p4))

# Q4.

def mul_interval_fast(x, y):
    """Return the interval that contains the product of any value in x and any
    value in y, using as few multiplications as possible.

    >>> str_interval(mul_interval_fast(interval(-1, 2), interval(4, 8)))
    '-8 to 16'
    >>> str_interval(mul_interval_fast(interval(-2, -1), interval(4, 8)))
    '-16 to -4'
    >>> str_interval(mul_interval_fast(interval(-1, 3), interval(-4, 8)))
    '-12 to 24'
    >>> str_interval(mul_interval_fast(interval(-1, 2), interval(-8, 4)))
    '-16 to 8'
    """
    x0, y0, x1, y1 = x[0], x[1], y[0], y[1]
    if x0>=0 and y0>=0 and x1>=0 and y1>0:
        return interval(min(x0, y0) * min(x1, y1), max(x0, y0) * max(x1, y1))
    elif y0>=0 and y1>=0:
        return interval( min( min(x0,y0) * max(x1, y1), max(x0, y0) * min(x1, y1)) , max(min(x0,y0) * max(x1, y1), max(x0, y0) * max(x1, y1)) )
    elif y0>=0 and y1<0:
        return interval(max(x0, y0) * min(x1, y1) , min(x0, y0) * min(x1, y1))
    elif y0<0 and y1>=0:
        return interval(min(x0,y0) * max(x1, y1) , max(x0, y0) * min(x1, y1))
    elif y0<0 and y1<0:
        return interval(max(x0, y0) * max(x1, y1) , min(x0, y0) * min(x1, y1))
    elif x0<0 and y0<0 and x1<0 and y1<0:
        return interval(max(x0, y0) * max(x1, y1), min(x0, y0) * min(x1, y1))
    elif x0>= 0 and x1<0:
        if y1>=0:
            return interval(max(x0, y0) * min(x1, y1), max(x0, y0) * max(x1, y1))
        else:
            return interval(max(x0, y0) * min(x1, y1), min(x0, y0) * max(x1, y1))
    elif x0<0 and x1>=0:
        if y0>=0:
            return interval(min(x0,y0) * max(x1, y1), max(x0, y0) * max(x1, y1))
        else:
            return interval(min(x0, y0) * max(x1, y1), max(x0, y0) * min(x1, y1))



# Q5.

def make_center_width(c, w):
    """Construct an interval from center and width."""
    return interval(c - w, c + w)

def center(x):
    """Return the center of interval x."""
    return (upper_bound(x) + lower_bound(x)) / 2

def width(x):
    """Return the width of interval x."""
    return (upper_bound(x) - lower_bound(x)) / 2


def make_center_percent(c, p):
    """Construct an interval from center and percentage tolerance.

    >>> str_interval(make_center_percent(2, 50))
    '1.0 to 3.0'
    """
    width = (c*p)/100
    return make_center_width(c, width)

def percent(x):
    """Return the percentage tolerance of interval x.

    >>> percent(interval(1, 3))
    50.0
    """
    return (100 * (width(x) / center(x)))

# Q6.

def par1(r1, r2):
    return div_interval(mul_interval(r1, r2), add_interval(r1, r2))

def par2(r1, r2):
    one = interval(1, 1)
    rep_r1 = div_interval(one, r1)
    rep_r2 = div_interval(one, r2)
    return div_interval(one, add_interval(rep_r1, rep_r2))


# These two intervals give different results for parallel resistors:
argument1  = make_center_percent(0.5 , 10)
argument2 = make_center_percent(0.5, 10)
print(par1(argument1, argument2),  par2(argument1, argument2))

# Q7.

def multiple_references_explanation():
    a = "Eva Lu Ator truly states thay par2 is much better that par1. First of all, the example above truly states that the return values of par1 and par2 can be different. Hence one of them gives some interval of error, which brings to the idea that the Mutiple References Problem is true. For instance par1(0.77, 0.88) and par2(0.77, 0.88) give slightly different results. A good case is that if there is a negative number in one interval bu the second number of the interval is positive. Lets define negative number is neg and positive num as pos such as abs(neg) =  pos.  Notice that in the par1() there is mdenominator can result to 0 that will create an  error "
    return a

# Q8.

def fun_creator(x, a, b, c):
    return a *(x**2) + b *x  + c

def quadratic(x, a, b, c):
    """Return the interval that is the range of the quadratic defined by
    coefficients a, b, and c, for domain interval x.

    >>> str_interval(quadratic(interval(0, 2), -2, 3, -1))
    '-3 to 0.125'
    >>> str_interval(quadratic(interval(1, 3), 2, -3, 1))
    '0 to 10'
    """
    arg1 = fun_creator(x[0], a, b, c)
    arg2 = fun_creator(x[1], a, b, c)
    arg3 = fun_creator(-b/(2*a), a, b, c)
    if -b/(2*a) <= x[1] and -b/(2*a) >= x[0]:
        return interval(min(arg1, arg2, arg3), max(arg1, arg2, arg3))
    else:
        return interval(min(arg1, arg2), max(arg1, arg2))

# Q9.

def non_zero(x):
    """Return whether x contains 0."""
    return lower_bound(x) > 0 or upper_bound(x) < 0

def square_interval(x):
    """Return the interval that contains all squares of values in x, where x
    does not contain 0.
    """
    assert non_zero(x), 'square_interval is incorrect for x containing 0'
    return mul_interval(x, x)

# The first two of these intervals contain 0, but the third does not.
seq = (interval(-1, 2), make_center_width(-1, 2), make_center_percent(-1, 50))

zero = interval(0, 0)

def sum_nonzero_with_for(seq):
    """Returns an interval that is the sum of the squares of the non-zero
    intervals in seq, using a for statement.

    >>> str_interval(sum_nonzero_with_for(seq))
    '0.25 to 2.25'
    """
    initial = (0,0)
    for i in seq:
        if non_zero(i) == True:
            initial = add_interval(initial, square_interval(i))
    return initial 

from functools import reduce
def sum_nonzero_with_map_filter_reduce(seq):
    """Returns an interval that is the sum of the squares of the non-zero
    intervals in seq, using using map, filter, and reduce.

    >>> str_interval(sum_nonzero_with_map_filter_reduce(seq))
    '0.25 to 2.25'
    """
    return reduce(add_interval, map(square_interval, filter(non_zero, seq)), (0,0))

def sum_nonzero_with_generator_reduce(seq):
    """Returns an interval that is the sum of the squares of the non-zero
    intervals in seq, using using reduce and a generator expression.

    >>> str_interval(sum_nonzero_with_generator_reduce(seq))
    '0.25 to 2.25'
    """
    return reduce(add_interval, (square_interval(x) for x in seq if non_zero(x)==True), (0,0))


# Q10.


def polynomial(x, c):
    """Return the interval that is the range of the polynomial defined by
    coefficients c, for domain interval x.

    >>> str_interval(polynomial(interval(0, 2), (-1, 3, -2)))
    '-3 to 0.125'
    >>> str_interval(polynomial(interval(1, 3), (1, -3, 2)))
    '0 to 10'
    >>> str_interval(polynomial(interval(0.5, 2.25), (10, 24, -6, -8, 3)))
    '18.0 to 23.0'
    """
    "*** YOUR CODE HERE ***"

