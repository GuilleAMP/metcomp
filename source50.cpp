#include <iostream>
#include <string>
using namespace::std;

class Empleado {
private:
    int id;
    string nombre;
    float salario;
public:

    Empleado() {
        id = 0;
        nombre = "Nadie";
        salario = 0.0;
    }

    Empleado(int i, float s) {
        id = i;
        salario = s;
	nombre = "Desconocido";
    }

    float getSalario() {
        return salario;
    }

    bool salarioAlto() {
        return salario > 10000;
    }

    void setNombre(string n) {
	nombre = n;
    }
};


int main() {
    Empleado e1;
    Empleado e2(1, 15000);
    e1.setNombre("Guille");

    if (e2.salarioAlto()) {
	cout << "Tiene buen salario\n";	
    } else {
        cout << "No tiene buen salario\n";
    }

    return 0;
}
