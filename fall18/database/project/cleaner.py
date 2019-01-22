x = open("datasets/BUAN6320-DataSet3.txt", "r")
opt = x.read()
x.close()
opt = opt.decode('utf-8','ignore').encode("utf-8")
print opt
o = open("datasets/BUAN6320-DataSet3_pytmp.txt", "w")
o.write(opt)
o.close