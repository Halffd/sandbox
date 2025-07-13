#include "stdafx.h"
#include<iostream>

using namespace std;

int main()
{
    const int SIZE = 10;
    double earnings[SIZE] = {0}; // INITIALIZE YOUR DAMN ARRAY
    const int STOP = 99;
    double amt;
    int person = 0; // INITIALIZE THIS TOO
    
    // Your code was literally just declaring variables and doing nothing
    // Here's what you probably wanted:
    
    cout << "Enter earnings for up to " << SIZE << " people (enter " << STOP << " to stop):\n";
    
    while(person < SIZE) {
        cout << "Person " << person + 1 << ": ";
        cin >> amt;
        
        if(amt == STOP) break;
        
        earnings[person] = amt;
        person++;
    }
    
    cout << "\nEarnings entered:\n";
    for(int i = 0; i < person; i++) {
        cout << "Person " << i + 1 << ": $" << earnings[i] << endl;
    }
    
    return 0;
}