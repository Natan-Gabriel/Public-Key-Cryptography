import secrets
import sys
import random

sys.setrecursionlimit(2000)


def generate_binary_number(n):
    if n == 0:
        return [0]
    l = []
    while n > 0:
        l.append(n % 2)
        n = n // 2
    return l
def rsme(b, k, n):
    a = 1
    if (k == 0):
        return a
    c = b
    l = generate_binary_number(k)
    if l[0] == 1:
        a = b
    for i in range(1, len(l)):
        c = c * c % n
        if l[i] == 1:
            a = c * a % n
    return a

def euclidean(a, b):
    if b>0:
        return euclidean(b, a % b)
    return a
def extended_euclidean(a, b):
    u2 = 1
    u1 = 0
    v2 = 0
    v1 = 1
    while b > 0:
        q = a // b
        r = a - q * b
        u = u2 - q * u1
        v = v2 - q * v1

        a = b
        b = r
        u2 = u1
        u1 = u
        v2 = v1
        v1 = v
    d = a
    u = u2
    v = v2
    return d, u, v


def miller_rabin_test_wrapper(n, k):
    t = n - 1
    s = 0
    while t % 2 == 0:
        t = t // 2
        s += 1
    while k > 0:
        result = miller_rabin_test(n, s, t)
        if not result:
            return False  # the result is composite
        k -= 1
    return True  # the result may be prime
def miller_rabin_test(n, s, t):
    a = secrets.randbelow(n - 2) + 2
    # now let's compute the sequence
    sequence = []
    a_t = rsme(a, t, n)
    sequence.append(a_t)
    for i in range(1, s + 1):
        a_t = a_t * a_t % n
        sequence.append(a_t)

    if sequence[0] == 1:
        return True
    for i in range(1, len(sequence)):
        if sequence[i] == 1:
            if sequence[i - 1] == n - 1:
                return True
            else:
                return False
    return False
def trivial_primality_check(number):
    for i in [2, 3, 5, 7, 11, 13, 17, 19]:
        if number % i == 0:
            return False
    return True
def generate_large_prime_wrapper(order):
    number = 2 ** order
    return generate_large_prime(number)
def generate_large_prime(number):
    random_number = secrets.randbelow(number - 1) + number + 1
    if not trivial_primality_check(random_number):
        return generate_large_prime(number)
    if not miller_rabin_test_wrapper(random_number, 50):
        return generate_large_prime(number)
    return random_number
def generate_key(order):
    p = generate_large_prime_wrapper(order)
    # print("p: ", p)
    q = generate_large_prime_wrapper(order)
    # sprint("q: ", q)
    while p == q:
        q = generate_large_prime_wrapper(order)
        print("q: ", q)
    n = p * q
    phi_n = (p - 1) * (q - 1)
    # secrets.randbelow(phi_n-2) generates a random in range [0,phi_n-2),then
    # secrets.randbelow(phi_n-2) + 2 generates a random in range [2,phi_n) ,that is(1,phi_n)
    e = secrets.randbelow(phi_n - 2) + 2
    while euclidean(e, phi_n) != 1:
        e = secrets.randbelow(phi_n - 2) + 2
    _, d, _ = extended_euclidean(e, phi_n)
    d = (d + phi_n) % phi_n
    # (n,e) is public key and d is private
    return n, e, d


alphabet = {" ": 0, "a": 1, "b": 2, "c": 3, "d": 4, "e": 5, "f": 6, "g": 7, "h": 8, "i": 9, "j": 10, "k": 11, "l": 12,
            "m": 13, "n": 14, "o": 15, "p": 16, "q": 17, "r": 18, "s": 19, "t": 20, "u": 21, "v": 22, "w": 23, "x": 24,
            "y": 25, "z": 26}

numbers = {0: " ", 1: "a", 2: "b", 3: "c", 4: "d", 5: "e", 6: "f", 7: "g", 8: "h", 9: "i", 10: "j", 11: "k", 12: "l",
           13: "m", 14: "n", 15: "o", 16: "p", 17: "q", 18: "r", 19: "s", 20: "t", 21: "u", 22: "v", 23: "w", 24: "x",
           25: "y", 26: "z"}

def compute_numerical_equivalent(text):
    numerical_equivalent = 0
    for i in text:
        numerical_equivalent = numerical_equivalent * 27 + alphabet[i]
    return numerical_equivalent
def compute_literal_equivalent(number, iterations):
    literal_equivalent = ""
    while iterations > 0:
        literal_equivalent = numbers[number % 27] + literal_equivalent
        number = number // 27
        iterations -= 1
    return literal_equivalent
def encrypt(plaintext, n, e, k, l):
    if 27 ** k >= n or n >= 27 ** l:
        return "Please choose some appropriate values for k and l"

    ciphertext = ""
    while len(plaintext) % k != 0:
        plaintext += numbers[0]

    for i in range(0, len(plaintext) // k):
        numerical_equivalent = compute_numerical_equivalent(
            plaintext[k * i:k * (i + 1)])
        encrypted_number = rsme(numerical_equivalent, e, n)
        literal_equivalent = compute_literal_equivalent(encrypted_number, l)
        ciphertext = ciphertext + literal_equivalent

    return ciphertext

def decrypt(ciphertext, n, d, k, l):
    if 27 ** k >= n or n >= 27 ** l:
        return "Please choose some appropriate values for k and l"
    plaintext = ""
    for i in range(0, len(ciphertext) // l):
        numerical_equivalent = compute_numerical_equivalent(ciphertext[l * i:l * (
                    i + 1)])
        decrypted_number = rsme(numerical_equivalent, d, n)
        literal_equivalent = compute_literal_equivalent(decrypted_number, k)
        plaintext = plaintext + literal_equivalent

    for i in range(len(plaintext) - 1, -1, -1):
        if plaintext[i] == numbers[0]:
            plaintext = plaintext[:-1]
        else:
            break

    return plaintext
def RSA(message, order=512, k=-1, l=-1):
    n, e, d = generate_key(order)
    lower_bound = 0.21030991785714
    upper_bound = 0.21030991785716
    if k == -1 or l == -1:
        # k = int(2 * order * lower_bound)
        # l = int(2 * (order + 1) * upper_bound) + 1
        k = random.randrange(2, int(2 * order * lower_bound)+1)
        aux = int(2 * (order + 1) * upper_bound) + 1
        l = random.randrange(aux, aux*4)
    print("Message to be encrypted: ", message)
    encrypted_message = encrypt(message, n, e, k, l)
    print("encrypted_message: ", encrypted_message)
    decrypted_message = decrypt(encrypted_message, n, d, k, l)
    print("decrypted_message: ", decrypted_message)
    
def RSA_using_file(file_name, order=512, k=-1, l=-1):
    n, e, d = generate_key(order)
    lower_bound = 0.21030991785714
    upper_bound = 0.21030991785716
    if k == -1 or l == -1:
        # k = int(2 * order * lower_bound)
        # l = int(2 * (order + 1) * upper_bound) + 1
        k = random.randrange(2, int(2 * order * lower_bound)+1)
        aux = int(2 * (order + 1) * upper_bound) + 1
        l = random.randrange(aux, aux*4)

    f = open(file_name, "r")
    message = f.read()
    f.close()

    print("Message to be encrypted: ", message)


    encrypted_message = encrypt(message, n, e, k, l)
    encrypted_file = file_name+".encrypted"
    f = open(encrypted_file, "w")
    f.write(encrypted_message)
    f.close()
    print("encrypted_message: ", encrypted_message)



    decrypted_message = decrypt(encrypted_message, n, d, k, l)
    decrypted_file = file_name + ".decrypted"
    f = open(decrypted_file, "w")
    f.write(decrypted_message)
    f.close()
    print("decrypted_message: ", decrypted_message)


def main():
    RSA("the best time to visit cancun is from december to april during the peak season")
    # RSA_using_file("message.txt")
def tests():
    assert miller_rabin_test_wrapper(101, 50)
    assert not miller_rabin_test_wrapper(123, 50)

    # testing miller_rabin_test_wrapper function (with 50 iterations)
    for i in [17, 19,31,61,89,107]:
        assert miller_rabin_test_wrapper(2**i - 1,50)
    for i in [21,29,49,80,99,123]:
        assert not miller_rabin_test_wrapper(2**i - 1, 50)

    # testing the extended_euclidean and euclidean functions
    for i in range(0, 20):
        a = random.randrange(10, 1000)
        b = random.randrange(10, 1000)
        l = extended_euclidean(a, b)
        assert a * l[1] + b * l[2] == euclidean(a, b)

    # testing the rsme function, which computes a^b mod n using repeated squaring modular exponentiation
    assert rsme(16, 10, 11) == pow(16, 10, 11)
    assert rsme(116, 107, 211) == pow(116, 107, 211)
    assert rsme(145, 129, 199) == pow(145, 129, 199)
    for i in range(0, 20):
        a = random.randrange(10, 1000)
        b = random.randrange(10, 1000)
        n = random.randrange(10, 1000)
        assert rsme(a, b, n) == pow(a, b, n)

    # testing the encrypt and decrypt functions

    for i in range(0, 20):
        # 27^ k >= n or n >= 27^ l
        message_length = random.randrange(10, 1000)
        characters = list(alphabet.keys())
        message = ''.join([random.choice(characters) for n in range(message_length)])
        # remove trailing spaces
        for i in range(len(message) - 1, -1, -1):
            if message[i] == numbers[0]:
                message = message[:-1]
            else:
                break
        order = 128
        n, e, d = generate_key(order)
        #lower bound is a little bit SMALLER than log 27 2
        # 27 ** k <= n and n <= 27 ** l
        # =>k * log 2 27 <= log 2 n and n <= 27 ** l and as we choose
        # n in interval 2^(order)+1,2^(order+1)-1 => k * log 2 27 <= log 2 n <= order   =>
        # it's safe to take k = (log 2 27)^(-1) * log 2 n = log 27 2 * log 2 n
        lower_bound = 0.21030991785714
        # lower bound is a little bit LARGER than log 27 2
        upper_bound = 0.21030991785716

        k = random.randrange(1, int(2 * order * lower_bound)+1)
        # aux is lower bound for l
        aux = int(2 * (order + 1) * upper_bound) + 1
        l = random.randrange(aux, aux*4)

        # print("Message to be encrypted: ", message)
        encrypted_message = encrypt(message, n, e, k, l)
        # print("encrypted_message: ", encrypted_message)
        decrypted_message = decrypt(encrypted_message, n, d, k, l)
        # print("message: ",message)
        # print("decrypted_message: ", decrypted_message)
        assert message == decrypted_message
    print("ALL TESTS PASSED")



tests()

main()

