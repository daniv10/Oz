# cook your dish here
def count_arr_reset(arr):   
    for i in range(9):
        arr[i] = 0
    return arr

def count_arr_valid(arr):
    for i in range(9):
        if arr[i]!=1:
            return False
    return True

def check_sudoku_is_valid(sud):
    sol = 1
    br = 0
    arr = [0,0,0,0,0,0,0,0,0]
    for row in sud:
        arr = count_arr_reset(arr)
        for elem in row:
            arr[elem-1] += 1;
        if not count_arr_valid(arr):
            return False
    for j in range(len(sud)):
        arr = count_arr_reset(arr)
        for row in sud:
            arr[row[j]-1] += 1
        if not count_arr_valid(arr):
            return False
    for m in range(3):
        for n in range(3):
            arr = count_arr_reset(arr)
            for x in range(3):
                for y in range(3):
                    arr[sud[m*3+x][n*3+y]-1] += 1;
            if not count_arr_valid(arr):
                return False
    return True
    
def test_example():
    #Valid
    sudoku1 = [[2,7,9,6,1,5,4,3,8], 
                [3,4,1,8,7,2,6,9,5],
                [5,6,8,3,4,9,1,2,7],
                [6,5,7,9,2,1,3,8,4],
                [1,8,4,5,3,7,9,6,2],
                [9,3,2,4,6,8,5,7,1],
                [4,2,3,7,5,6,8,1,9],
                [8,1,6,2,9,4,7,5,3],
                [7,9,5,1,8,3,2,4,6]]
    if check_sudoku_is_valid(sudoku1):
        print ("Suduku1 is valid")
    else:
        print ("Suduku1 is not valid")

    #not valid 
    suduku2 = [[2,7,9,6,1,5,4,3,8], 
                [3,4,1,8,7,2,6,9,5],
                [5,6,8,3,4,9,1,2,7],
                [6,5,7,9,2,5,3,8,4],
                [1,8,4,5,3,7,9,6,2],
                [9,3,2,4,6,8,5,7,1],
                [4,2,3,7,5,6,8,1,9],
                [8,1,6,2,9,4,7,5,3],
                [7,9,5,1,8,3,2,4,6]]

    if check_sudoku_is_valid(suduku2):
        print ("Suduku2 is valid")
    else:
        print ("Suduku2 is not valid")

if __name__ == '__main__':
    test_example()
