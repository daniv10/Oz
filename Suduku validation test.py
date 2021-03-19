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
    sudoku1 = [[5, 3, 4, 6, 7, 8, 9, 1, 2], 
                [6, 7, 2, 1, 9, 5, 3, 4, 8],
                [1, 9, 8, 3, 4, 2, 5, 6, 7],
                [8, 5, 9, 7, 6, 1, 4, 2, 3],
                [4, 2, 6, 8, 5, 3, 7, 9, 1],
                [7, 1, 3, 9, 2, 4, 8, 5, 6],
                [9, 6, 1, 5, 3, 7, 2, 8, 4],
                [2, 8, 7, 4, 1, 9, 6, 3, 5],
                [3, 4, 5, 2, 8, 6, 1, 7, 9]]
    if check_sudoku_is_valid(sudoku1):
        print ("Suduku1 is valid")
    else:
        print ("Suduku1 is not valid")

    #not valid 
    suduku2 = [[5, 3, 4, 6, 7, 8, 9, 1, 2],
                [6, 7, 2, 1, 9, 5, 3, 4, 8],
                [1, 9, 8, 3, 4, 2, 5, 6, 7],
                [8, 5, 9, 7, 6, 1, 4, 2, 3],
                [4, 2, 6, 8, 5, 3, 7, 9, 1],
                [7, 1, 3, 9, 2, 4, 8, 5, 6],
                [9, 6, 1, 5, 3, 7, 2, 8, 4],
                [2, 8, 7, 4, 1, 9, 4, 3, 5],
                [3, 4, 5, 2, 8, 6, 1, 7, 9]]

    if check_sudoku_is_valid(suduku2):
        print ("Suduku2 is valid")
    else:
        print ("Suduku2 is not valid")

if __name__ == '__main__':
    test_example()