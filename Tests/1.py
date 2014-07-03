from operator import add, mul

def square(x):
    """Return x squared."""
    return x * x

def iden(x):
    return x

# Q1

def product(n, term):
    """Return the product of the first n terms in a sequence.

    term -- a function that takes one argument

    >>> product(4, square)
    576
    """
    k, total = 1, 1
    while k<n+1:
        total = total * term(k)
        k=k+1
    return total


def factorial(n):
    """Return n factorial for n >= 0 by calling product.

    >>> factorial(4)
    24
    """
    return product(n, iden)

# Q2

def accumulate(combiner, start, n, term):
    """Return the result of combining the first n terms in a sequence."""
    k, total = start, 1
    while k<=n:
        total=combiner(total, term(k))
        k=k+1
    return total
    


def summation_using_accumulate(n, term):
    """An implementation of summation using accumulate.

    >>> summation_using_accumulate(4, square)
    30
    """
    return accumulate(add, 0, n, term)-1

def product_using_accumulate(n, term):
    """An implementation of product using accumulate.

    >>> product_using_accumulate(4, square)
    576
    """
    return accumulate(mul, 1, n, term)

# Q3

def double(f):
    """Return a function that applies f twice.

    f -- a function that takes one argument

    >>> double(square)(2)
    16
    """
    def g(x):
        return f(f(x))
    return g


# Q4

def compose1(f, g):
    """Return a function h, such that h(x) = f(g(x))."""
    def h(x):
        return f(g(x))
    return h

def repeated(f, n):
    """Return the function that computes the nth application of f.

    f -- a function that takes one argument
    n -- a positive integer

    >>> repeated(square, 2)(5)
    625
    >>> repeated(square, 4)(5)
    152587890625
    """
    def number (x):
        result = x
        if n%2 == 0:
            counter = n
            while counter>0:
                result = compose1(f,f)(result) 
                counter = counter - 2
        if n%2==1:
            counter = n
            while counter>0:
                result = f(result)
                counter=counter -1
        return result
    return number

    
