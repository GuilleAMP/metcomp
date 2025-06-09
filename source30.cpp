#include <iostream>
using namespace::std;

int main() {
    int n;

    cout << "Ingresa un numero: ";
    cin >> n;

    if (n < 0) {
        return 0;
    }

    for (int i=1; i<=n; i++) {
	if (i % 2 == 0) {
	    cout << "Es par\n";
	} else {
	    cout << "Es impar\n";
	}
    }
    return 0;
}
