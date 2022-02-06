import JSON

tab = data.frame(id = 1:5, name = ["hello","world","me","this","self"], flags = [True, False, False, True, True])

print(tab)

list = list(mat = tab, names = ["a","b","c"], i = 95:100, flags = list(name = True, value = "xxx"))
json = json_encode(list)

print(json)

jsonfile = @dir & "/text.json"

print(jsonfile)

writeLines(json, con = jsonfile)