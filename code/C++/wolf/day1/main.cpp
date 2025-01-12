#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <cmath>
#include <sstream>

// Function to calculate the "distance" between two lists
int distanceBetweenLists(const std::vector<int>& left, const std::vector<int>& right) {
    // Step 1: Sort both lists
    std::vector<int> sortedLeft = left;
    std::vector<int> sortedRight = right;

    std::sort(sortedLeft.begin(), sortedLeft.end());
    std::sort(sortedRight.begin(), sortedRight.end());

    // Step 2: Calculate the sum of absolute differences
    int distance = 0;
    for (size_t i = 0; i < sortedLeft.size(); ++i) {
        distance += std::abs(sortedLeft[i] - sortedRight[i]);
    }

    return distance;
}

// Function to calculate the "similarity score" between two lists
int similarityScore(const std::vector<int>& left, const std::vector<int>& right) {
    // Step 1: Count occurrences in the "right" list
    std::map<int, int> frequencyMap;
    for (int num : right) {
        ++frequencyMap[num];
    }

    // Step 2: Calculate the similarity score based on the "left" list
    int score = 0;
    for (int num : left) {
        score += num * frequencyMap[num];
    }

    return score;
}

// Function to read two lists of integers from input
std::pair<std::vector<int>, std::vector<int> > readLists() {
    std::vector<int> left, right;
    std::string line;

    while (std::getline(std::cin, line)) {
        std::istringstream iss(line);
        int l, r;
        if (!(iss >> l >> r)) {
            break; // End of input
        }
        left.push_back(l);
        right.push_back(r);
    }

    return {left, right};
}

// Main function
int main() {
    // Read input lists
    auto [left, right] = readLists();

    // Calculate results
    int distance = distanceBetweenLists(left, right);
    int similarity = similarityScore(left, right);

    // Print results
    std::cout << "Day 1, part 1: the distance between the two lists is " << distance << "." << std::endl;
    std::cout << "Day 1, part 2: the similarity score for the two lists is " << similarity << "." << std::endl;

    return 0;
}
