#!/bin/bash

# Extract left and right numbers from list.txt
left_numbers=$(awk '{print $1}' list.txt | xargs)
right_numbers=$(awk '{print $2}' list.txt | xargs)

# Check if both lists have the same number of elements
num_left=$(echo "$left_numbers" | wc -w)
num_right=$(echo "$right_numbers" | wc -w)
if [ $num_left -ne $num_right ]; then
    echo "Error: Lists have different number of elements."
    exit 1
fi

# Sort the left and right lists numerically
sorted_left=( $(echo "$left_numbers" | tr ' ' '\n' | sort -n) )
sorted_right=( $(echo "$right_numbers" | tr ' ' '\n' | sort -n) )

total_distance=0

# Calculate the total distance by summing absolute differences
for i in ${!sorted_left[@]}; do
    # Compute the difference (could be positive or negative initially)
    diff=$(( ${sorted_left[$i]} - ${sorted_right[$i]} ))
    # Ensure the distance is non-negative by taking the absolute value
    if [ $diff -lt 0 ]; then
        abs_diff=$((-diff))  # If negative, flip to positive
    else
        abs_diff=$diff       # If positive or zero, use as is
    fi
    # Add the non-negative distance to the total
    total_distance=$(( total_distance + abs_diff ))
done

echo $total_distance
