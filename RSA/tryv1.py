import secrets

def euclidean(a,b):
    if(a==0):
        return b
    return euclidean(b%a,a)


def generateLargePrime():
    return 17

def generateKey():
    p = generateLargePrime()
    q = generateLargePrime() 
    n=p*q
    phi_n=(p-1)*(q-1)
    #secrets.randbelow(phi_n-2) generates a random in range [0,phi_n-2),then
    #secrets.randbelow(phi_n-2) + 2 generates a random in range [2,phi_n) ,that is(1,phi_n)
    e=secrets.randbelow(phi_n-2) + 2
    while euclidean(e,phi_n)!=1:
        e = secrets.randbelow(phi_n - 2) + 2
    res1,d,res2=extendedEuclidean(e,phi_n)

    # now (n,e) is public key and d is private




def extendedEuclidean(a,b):
    u2=1
    u1=0
    v2=0
    v1=1
    while b>0:
        q=a//b
        r=a-q*b
        u=u2-q*u1
        v=v2-q*v1

        a=b
        b=r
        u2=u1
        u1=u
        v2=v1
        v1=v
    d=a
    u=u2
    v=v2
    return d,u,v

extendedEuclidean(67, 1560)

def generateBinaryNumber(n):
    if n==0:
        return [0]
    l=[]
    while n>0:
        l.append(n%2)
        n=n//2
    return l

print(generateBinaryNumber(11))


def rsme(b,k,n):
    a=1
    if(k==0):
        return a
    c=b
    l=generateBinaryNumber(k)
    #print(k,l)
    if l[0]==1:
        a=b
    for i in range(1,len(l)):
        c=c*c % n
        if l[i]==1:
            a=c*a % n
    return a

assert(rsme(16,10,11)==16**10 % 11)
assert(rsme(116,107,211)==116**107 % 211)
assert(rsme(145,129,199)==145**129 % 199)

print(rsme(39,67,1643))
print(39**67 % 1643)

# k=2




# alphabet={0:"_",1:"A",2:"B", 3: "C",4: "D",5: "E" ,6: "F",7: "G",8: "H",9: "I",10: "J" ,11:"K",12: "L",13: "M", 14: "N", 15: "O",16: "P",17: "Q",18: "R",19: "S",20: "T",21: "U",22: "V",23: "W", 24: "X", 25: "Y",26: "Z"}
numbers={0:"_",1:"a",2:"b", 3: "c",4: "d",5: "e" ,6: "f",7: "g",8: "h",9: "i",10: "j" ,11:"k",12: "l",13: "m", 14: "n", 15: "o",16: "p",17: "q",18: "r",19: "s",20: "t",21: "u",22: "v",23: "w", 24: "x", 25: "y",26: "z"}
alphabet={"_":0,"a":1,"b":2, "c":3, "d":4, "e":5 , "f":6, "g":7, "h":8, "i":9, "j":10 ,"k":11, "l":12, "m":13,  "n":14, "o":15, "p":16, "q":17, "r":18, "s":19, "t":20, "u":21, "v":22, "w":23, "x":24, "y":25, "z":26}

def encrypt(plaintext,n,e):
    ciphertext = ""
    while len(plaintext) % 2 != 0:
        plaintext += "_"
    #Write the numerical equivalents
    for i in range(0, len(plaintext)//2):
        numerical_equivalent = alphabet[plaintext[2 * i]]*27+alphabet[plaintext[2 * i + 1]]
        # print("numerical_equivalent: ",numerical_equivalent)
        encrypted_number = rsme(numerical_equivalent, e, n)
        print("encrypted_number: ", encrypted_number)
        third_encrypted_letter = numbers[encrypted_number % 27]
        encrypted_number = encrypted_number // 27
        second_encrypted_letter = numbers[encrypted_number % 27]
        encrypted_number = encrypted_number // 27
        first_encrypted_letter = numbers[encrypted_number]
        ciphertext = ciphertext + first_encrypted_letter + second_encrypted_letter + third_encrypted_letter

    return ciphertext

def decrypt(ciphertext,n,d):
    plaintext = ""
    for i in range(0,len(ciphertext)//3):
        numerical_equivalent = alphabet[ciphertext[3 * i]] * (27**2) + alphabet[ciphertext[3 * i + 1]] * 27 + alphabet[ciphertext[3 * i + 2]]
        print("numerical_equivalent: ",numerical_equivalent)
        decrypted_number = rsme(numerical_equivalent, d, n)
        print("decrypted_number: ",decrypted_number)
        second_decrypted_letter = numbers[decrypted_number % 27]
        decrypted_number = decrypted_number // 27
        print(decrypted_number)
        first_decrypted_letter = numbers[decrypted_number]
        plaintext = plaintext + first_decrypted_letter + second_decrypted_letter
    return plaintext

def main():
    encrypted_message = encrypt("algebra", 1643, 67)
    print(encrypted_message)
    decrypted_message = decrypt(encrypted_message, 1643, 163)
    print(decrypted_message)

main()