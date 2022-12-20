def add(x, y):
    return x + y


def test(x, y):
    if x < 0.0:
        return add(x, y)
    elif x > 0.0:
        return x + y
