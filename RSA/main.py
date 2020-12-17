import secrets
import sys
print("sys.getrecursionlimit(): ",sys.getrecursionlimit())
sys.setrecursionlimit(3000)

def euclidean(a, b):
    if (a == 0):
        return b
    return euclidean(b % a, a)


def generate_binary_number(n):
    if n == 0:
        return [0]
    l = []
    while n > 0:
        l.append(n % 2)
        n = n // 2
    return l


# print(generate_binary_number(11))

def rsme_wrapper(b, k, n):
    if k >= 0:
        return rsme(b, k, n)
    k = (-1) * k
    res = rsme(b, k, n)
    # _, res, _ = extended_euclidean(res, n)
    return res


def rsme(b, k, n):
    a = 1
    if (k == 0):
        return a
    c = b
    l = generate_binary_number(k)
    # print(k,l)
    # print(l)
    if l[0] == 1:
        a = b
    for i in range(1, len(l)):
        c = c * c % n
        if l[i] == 1:
            a = c * a % n
    return a


assert (rsme(16, 10, 11) == 16 ** 10 % 11)
assert (rsme(116, 107, 211) == 116 ** 107 % 211)
assert (rsme(145, 129, 199) == 145 ** 129 % 199)


# print(rsme(39,67,1643))
# print(39**67 % 1643)

def miller_rabin_test_wrapper(n, k):
    t = n - 1
    s = 0
    while t % 2 == 0:
        t = t // 2
        s += 1
    # print("aici",s,t)
    while k > 0:
        result = miller_rabin_test(n, s, t)
        k -= 1
        if not result:
            return False  # the result is composite
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
    # print("sequence: ",sequence)
    for i in range(1, len(sequence)):
        if sequence[i] == 1:
            if sequence[i - 1] == n - 1:
                return True
            else:
                return False
    return False


print("miller_rabin_test_wrapper(100, 50):", miller_rabin_test_wrapper(101, 50))
# print("miller_rabin_test(101, 2,25):",miller_rabin_test(409, 3,51))

assert miller_rabin_test_wrapper(101, 50)
assert not miller_rabin_test_wrapper(123, 50)
assert miller_rabin_test_wrapper(2 ** 17 - 1, 50)
assert miller_rabin_test_wrapper(2 ** 19 - 1, 50)
assert not miller_rabin_test_wrapper(2 ** 21 - 1, 50)
assert not miller_rabin_test_wrapper(2 ** 29 - 1, 50)
assert miller_rabin_test_wrapper(2 ** 31 - 1, 50)
assert not miller_rabin_test_wrapper(2 ** 49 - 1, 50)
assert miller_rabin_test_wrapper(2 ** 61 - 1, 50)
assert not miller_rabin_test_wrapper(2 ** 80 - 1, 50)


def trivial_primality_check(number):
    for i in [2, 3, 5, 7, 11, 13, 17, 19]:
        if number % i == 0:
            return False
    return True


print(trivial_primality_check(100))
print(trivial_primality_check(180))
print(trivial_primality_check(160))


# generate_large_prime function will generate a random prime number between 2^order+1 and 2^(order+1)-1
# order represents the number of bits of the number
# secrets.randbelow(2**order - 1) generates a number from interval [0,2**order - 1) =>
# secrets.randbelow(2**order - 1) + 2**order + 1 generates a number from interval [2**order + 1,2**order + 2**order) =>
# a number from interval [2**order + 1,2**(order+1) -1]
def generate_large_prime_wrapper(order):
    number = 2 ** order
    return generate_large_prime(number)


def generate_large_prime(number):
    random_number = secrets.randbelow(number - 1) + number + 1
    if not trivial_primality_check(random_number):
        return generate_large_prime(number)
    if not miller_rabin_test_wrapper(random_number, 50):
        return generate_large_prime(number)
    # print("not trivial_primality_check(random_number): ",random_number,not trivial_primality_check(random_number))
    return random_number


def generate_key(order):
    p = generate_large_prime_wrapper(order)
    print("p: ", p)
    q = generate_large_prime_wrapper(order)
    print("q: ", q)
    while p == q:
        q = generate_large_prime_wrapper(6)
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
    print(n, e, d, p, q)
    return n, e, d


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


extended_euclidean(67, 1560)

# k=2


# alphabet={0:"_",1:"A",2:"B", 3: "C",4: "D",5: "E" ,6: "F",7: "G",8: "H",9: "I",10: "J" ,11:"K",12: "L",13: "M", 14: "N", 15: "O",16: "P",17: "Q",18: "R",19: "S",20: "T",21: "U",22: "V",23: "W", 24: "X", 25: "Y",26: "Z"}
numbers = {0: " ", 1: "a", 2: "b", 3: "c", 4: "d", 5: "e", 6: "f", 7: "g", 8: "h", 9: "i", 10: "j", 11: "k", 12: "l",
           13: "m", 14: "n", 15: "o", 16: "p", 17: "q", 18: "r", 19: "s", 20: "t", 21: "u", 22: "v", 23: "w", 24: "x",
           25: "y", 26: "z"}
alphabet = {" ": 0, "a": 1, "b": 2, "c": 3, "d": 4, "e": 5, "f": 6, "g": 7, "h": 8, "i": 9, "j": 10, "k": 11, "l": 12,
            "m": 13, "n": 14, "o": 15, "p": 16, "q": 17, "r": 18, "s": 19, "t": 20, "u": 21, "v": 22, "w": 23, "x": 24,
            "y": 25, "z": 26}


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


# print(compute_numerical_equivalent("al"))
gg = "algebra"


# print(compute_numerical_equivalent(gg[2:4]))
# print(compute_literal_equivalent(1503,3))


# Plaintext message units are blocks of k letters, whereas
# ciphertext message units are blocks of l letters. The plaintext
# is completed with blanks, when necessary.

def encrypt(plaintext, n, e, k, l):
    if 27 ** k >= n or n >= 27 ** l:
        return "Please choose some appropriate values for k and l"

    ciphertext = ""
    while len(plaintext) % k != 0:
        plaintext += numbers[0]
    # Write the numerical equivalents
    for i in range(0, len(plaintext) // k):
        numerical_equivalent = compute_numerical_equivalent(
            plaintext[k * i:k * (i + 1)])  # alphabet[plaintext[2 * i]]*27+alphabet[plaintext[2 * i + 1]]
        # print("numerical_equivalent: ",numerical_equivalent)
        encrypted_number = rsme_wrapper(numerical_equivalent, e, n)
        # print("encrypted_number: ", encrypted_number)
        literal_equivalent = compute_literal_equivalent(encrypted_number, l)
        ciphertext = ciphertext + literal_equivalent

    return ciphertext


# In the decrypt function we won't have any case in which we have to complete the ciphertext with blanks,
# because the encrypt function always return a ciphertext of length multiple of "l"

def decrypt(ciphertext, n, d, k, l):
    # we MUST have 27^k < n < 27^l
    if 27 ** k >= n or n >= 27 ** l:
        return "Please choose some appropriate values for k and l"

    # print("ciphertext: ",ciphertext)
    plaintext = ""
    for i in range(0, len(ciphertext) // l):
        numerical_equivalent = compute_numerical_equivalent(ciphertext[l * i:l * (
                    i + 1)])  # alphabet[ciphertext[3 * i]] * (27**2) + alphabet[ciphertext[3 * i + 1]] * 27 + alphabet[ciphertext[3 * i + 2]]
        # print("numerical_equivalent: ",numerical_equivalent)
        decrypted_number = rsme_wrapper(numerical_equivalent, d, n)
        # print("decrypted_number: ",decrypted_number)
        literal_equivalent = compute_literal_equivalent(decrypted_number, k)
        plaintext = plaintext + literal_equivalent
    for i in range(len(plaintext) - 1, -1, -1):
        if plaintext[i] == "_":
            plaintext = plaintext[:-1]
    return plaintext


def RSA(message, order=1024, k=-1, l=-1):
    n, e, d = generate_key(order)
    lower_bound = 0.21030991785714
    upper_bound = 0.21030991785716
    if k == -1 or l == -1:
        k = int(2 * order * lower_bound)
        l = int(2 * (order + 1) * upper_bound) + 1
    # print(n,e,d)
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
        k = int(2 * order * lower_bound)
        l = int(2 * (order + 1) * upper_bound) + 1
    # print(n,e,d)

    f = open(file_name, "r")
    message = f.read()
    f.close()

    print("Message to be encrypted: ", message)


    encrypted_message = encrypt(message, n, e, k, l)
    encrypted_file = file_name+".encrypted"
    # print(encrypted_file)
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




# RSA("the best time to visit cancun is from december to april during the peak season")

# RSA_using_file("message.txt")


def main():
    message = "algebra"
    print("Message to be encrypted: ", message)
    encrypted_message = encrypt("algebra", 1643, 67, 2, 3)
    print("encrypted_message: ", encrypted_message)
    decrypted_message = decrypt(encrypted_message, 1643, 163, 2, 3)
    print("decrypted_message: ", decrypted_message)




# main()
