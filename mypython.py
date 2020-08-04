import csv

def hd(lis):
    return(lis[0])

def tl(lis):
    return(lis[1:])

def last(lis):
    return(lis [-1])

def reverse(lis1):
    lis2=[]
    for item in lis1:
        lis2=[item]+lis2
    return(lis2)

#get_data_set takes the names of a csv file and converts that file into a lists of lists which it returns
# it has a second argument which indicates whether the file has a header or not
#apprently - for reasons that I dont understand - dates in the format 'dd/mm/yyyy' get converted to 'yyyy-mm-dd'
def get_data_set(source_file_name,has_header):
    header=[]
    try:
        with open(source_file_name, 'r') as f:
            reader = csv.reader(f, delimiter=',')
            if has_header:
                header = next(reader)
            dataset = []
            for lis in reader:
                    dataset.append([x for x in lis])
    
    except IOError:
        print("\nFile "+source_file_name+" not found: A new file will be created.")
        return(False)
    return([header, dataset])
    
#print(get_data_set('ghs3.csv',True))

#get_data_set_header assumes that its argument is csv file which has a set of headers as its first row.
def get_data_set_header(source_file_name):
    try:
        with open(source_file_name, 'r') as f:
            reader = csv.reader(f, delimiter=',')
            header = next(reader)
    
    except IOError:
        print("\nFile "+source_file_name+" not found: A new file will be created.")
        return(False)
    return(header)


def write_out_dataset(oname,ds):
   ofile = open(oname, 'w')
   for lis in ds:     
        ofile.write(convert_list_to_csv(lis)+"\n")

def convert_list_to_csv(lis):
    string1=""
    for item in lis:
        string1=string1+str(item)+','
    return(string1[:-1])


def get_back_of_big_csv_file(input_file,output_file,n):
    count=0
    ofile = open(output_file, 'w')
    try:
        with open(input_file, 'r') as f:
            reader = csv.reader(f, delimiter=',')
            for lis in reader:
                count+=1
                if count>n:
                    ofile.write(convert_list_to_csv(lis)+"\n")
    
    except IOError:
        print("\nFile "+source_file_name+" not found: A new file will be created.")
        return(False)

def add_to_sorted_list_of_lists(item,sorted_list):
    #This needs checking.
    front=[]
    back=sorted_list
    while not(back == []):
        if back==[] or list_less_than(item,back[0]):
            front.append(item)
            front.extend(back)
        return (back)
    else:
        front.append(item)
        back=back[1:]
        

def list_less_than(lis1,lis2):
   #no checsk fro data types
   #no lnegth test
   for n in range(len(lis1)):
       if lis1[n]>=lis2[n]:
           return(false)
   return(true)