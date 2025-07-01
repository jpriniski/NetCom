# Print matrix elements
def showArray(arr, M, N):
    for i in range(M):
        for j in range(N):
            print(arr[i][j], end=" ")
        print()


# Function to shuffle matrix
def reverseAlternate(arr, K, M, N):
    turn = 0
    row = 0
    col = 0
    while (turn < K):

        # Reverse the row
        if (turn % 2 == 0):
            start = 0
            end = N - 1
            temp = 0
            while (start < end):
                temp = arr[row % M][start]
                arr[row % M][start] = arr[row % M][end]
                arr[row % M][end] = temp
                start += 1
                end -= 1

            row += 1
            turn += 1

        # Reverse the column
        else:
            start = 0
            end = M - 1
            temp = 0
            while (start < end):
                temp = arr[start][col % N]
                arr[start][col % N] = arr[end][col % N]
                arr[end][col % N] = temp
                start += 1
                end -= 1

            col += 1
            turn += 1
