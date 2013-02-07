import unittest
from functools import wraps

class typed:
    "Adds strong typing to decorated function"
    def __init__(self, *types, **kws):
        # set everything as a type or None
        types = list(types)
        for i, t in enumerate(types):
            if t is not None and not isinstance(t, type):
                types[i] = type(t)

        self._types = types

        self._returns = kws.get("returns")
        if self._returns is not None and not isinstance(self._returns, type):
            self._returns = type(self._returns)


    def __call__(self, func):
        # no point in wrapping if there are no arguments
        if len(self._types) == 0 and self._returns is None: return func 

        # otherwise, start inspecting
        code = func.func_code

        # make sure there is the right number of args
        if len(self._types) != code.co_argcount:
            raise AttributeError('typed: incorrect number of types, should have %d, received %d' % 
                                 (code.co_argcount, len(self._types)))


        # store the defaults for later
        varnames = code.co_varnames
        vartypes = zip(varnames, self._types)
        dflts = dict(zip(reversed(varnames[:code.co_argcount]), func.func_defaults or []))

        @wraps(func)
        def wrapper(*a, **kw):
            args = dflts.copy()
            args.update(kw)
            args.update(zip(varnames, a))

            for name,typ in vartypes:
                v = args.get(name)
                if typ is not None and v is not None and not isinstance(v, typ):
                    raise AttributeError("typed (%s): %s expects a %r, received a %r" % 
                                         (func.func_name, name, typ, type(v)))

            result = func(*a, **kw)

            if self._returns is not None and result is not None and not isinstance(result, self._returns):
                raise ValueError("typed (%s): %s should return a %r, instead returned a %r" % 
                                  (func.func_name, name, self._returns, type(result)))
            return result


        return wrapper



###############  UNIT TESTS #################

class TestTypedMethod(unittest.TestCase):
    def test_works_with_basic_funcs(self):
        @typed(str)
        def say_hi(name):
            return "Hi, " + name

        self.assertEqual("Hi, Steve", say_hi("Steve"))

        @typed(int, int)
        def mult(x, y): 
            return x*y

        self.assertEqual(10, mult(5,2))


    def test_allows_none_for_any_type(self):
        @typed(None, int)
        def repeat(obj, n):
            return [ obj for i in range(n) ]

        info = {} 
        self.assertEqual([info, info, info], repeat(info, 3))
        info = []
        self.assertEqual([info, info, info], repeat(info, 3))


    def test_allows_none_for_any_value(self):
        @typed(int, int)
        def posn(x,y):
            return [x,y]
        self.assertEqual([4, None], posn(4,None))

    def test_number_of_arguments_must_match(self):
        with self.assertRaises(AttributeError):
            @typed(int, int, float, int)
            def too_few_args(a):
                pass

        with self.assertRaises(AttributeError):
            @typed(int)
            def too_many_args(a,b,c,d,e):
                pass


    def test_can_use_instances_instead_of_types(self):
        @typed({}, [])
        def f(dct, lst):
            return zip(dct.keys(), lst)

        self.assertEqual([("a", 0)], f({"a": 3}, [0]))


    def test_errors_with_wrong_types(self):
        @typed(str)
        def say_hi(name):
            return "hi "+name

        with self.assertRaises(AttributeError):
            say_hi(42)


    def test_can_work_with_classes(self):
        class Rect:
            @typed(None, int, int)
            def __init__(self, w, h):
                self.w = w
                self.h = h

            @typed(None, returns=int)
            def area(self):
                return self.w * self.h

        self.assertEqual(15, Rect(3,5).area())

        with self.assertRaises(AttributeError):
            r = Rect("shouldn't", "work")

        with self.assertRaises(ValueError):
            r = Rect(3,4)
            r.w = 3.5
            r.area()

        with self.assertRaises(ValueError):
            r = Rect(3,4)
            r.w = "apple"
            r.area()


    def test_can_take_an_optional_return_value(self):
        @typed(str, returns=str)
        def hi(name):
            return "hi "+name
        self.assertEqual("hi bob", hi("bob"))

        @typed(int, float, returns=float)
        def mult(x,y):
            return x*y
        self.assertEqual(10.0, mult(2, 5.0))


    def test_invalid_returns(self):
        with self.assertRaises(ValueError):
            @typed(int, returns=int)
            def wrong(x):
                return "bad"

            wrong(3)

    def test_can_use_instance_instead_of_types_for_return(self):
        @typed(returns=[])
        def reverse(*a):
            return [ v for v in reversed(a) ]
        self.assertEqual([3,2,1], reverse(1,2,3))

    def test_returns_function_if_empty(self):
        def f(): pass
        g = typed()(f)
        self.assertIs(f, g)


if __name__ == "__main__":
    unittest.main()
