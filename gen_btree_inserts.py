for i in range(26):
    for j in range(26):
        a = chr(i+ord('A'))
        b = chr(j+ord('A'))
        print(f"Btree.insert t (Btree.Varchar \"{a+b}\") 9999;")
