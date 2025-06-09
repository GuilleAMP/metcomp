#include <iostream>
using namespace::std;

int main() {
    int total = 0;

    for (int i = 1; i <= 5; i++) {
        int cuadrado = i * i;
   	cout << cuadrado;
	cout << "\n";
    }


    int j = 1;
    while (j <= 5) {
        int cuadrado = j * j;
	cout << cuadrado;
	cout << "\n";
        j = j + 1;
    }

    return 0
}
