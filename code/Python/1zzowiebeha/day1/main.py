import os

# todo: tests,
#       data safety,
#       use generators,
#       user-friendly missing file message
#       test file-type bytes to validate data file

############
# Settings #
# Place the data file in the same directory as the program file #

DATA_FILENAME = 'data.txt'
BASE_FILE_PATH = os.path.join(os.path.dirname(__file__))
DATA_FILE_PATH = os.path.join(BASE_FILE_PATH, DATA_FILENAME)

############


def parse_data() -> tuple[list, list]:
    """Parse a space-deliminated 2-column file of integers into two lists."""
    list1 = []
    list2 = []
    with open(DATA_FILE_PATH, 'r') as file_object:
        for line in file_object.readlines():
            data = line.split(' ')
            
            # may fail on non-numeric non-conforming data
            list1.append(int(data[0]))
            list2.append(int(data[-1].strip()))
        
    return (list1, list2)


def caclulate_differences(l1: list, l2: list) -> int:
    """For each smallest value of each list,
    pop, and find the difference between the two.
    Print the sum of all differences."""
    sum = 0

    for iteration in range(len(l1)):
        # possible todo?: turn into generator
        list1_nextmin = l1.pop( l1.index(min(l1)) )
        list2_nextmin = l2.pop( l2.index(min(l2)) )
            
        pair = ( list1_nextmin, list2_nextmin )
            
        difference = max(pair) - min(pair)
            
        sum += difference
        
        
    print(f"Sum of all differences: {sum}")
    
    
if __name__ == "__main__":
    list1, list2 = parse_data()
    caclulate_differences(list1, list2)