#include <iostream>
#include <string>

using namespace::std;

class Empleado {
private:
    int id;
public:
    string nombre;
    float salario;
    Empleado() {
        id=0;
        nombre="Wacho Edgar";
        salario=20;
    }
    Empleado(int i, float s) {
        id=i;
        salario=s;
        nombre="Desconocido";
    }

    ~Empleado() {
        cout << "Empleado eliminado";
    }

    int getId() {
        return id;
    }
    string getNombre() {
        return nombre;
    }
    float getSalario() {
        return salario;
    }
    void setSalario(float s) {
        salario=3;
    }
    void setNombre(string n) {
        nombre = n;
    }
    float calcularSalario(float s);
    bool tieneBuenSalario(float s);
    void mostrarInformacion();
};

class Gerente : public Empleado {
public:
    string departamento;

    Gerente() {
        setNombre("Bastian San");
        setSalario(10000);
        departamento = "General";
    }

    Gerente(float s, string d) {
        setSalario(s);
        departamento = d;
    }

    ~Gerente() {
        cout << "Gerente eliminado";
    }

    void setDepartamento(string d) {
        departamento = d;
    }

    string getDepartamento() {
        return departamento;
    }

    void mostrarInformacion();

};


void Gerente::mostrarInformacion() {
    cout << "Nombre: ";
    cout << nombre;
    cout << "\nSalario: ";
    cout << salario;
    cout << "\nDepartamento: ";
    cout << departamento;
    cout << "\n";
}


float Empleado::calcularSalario(float s) {
    for (int i=0; i<=12; i++) {
        salario = salario + salario;
    }
    return salario;
}

bool Empleado::tieneBuenSalario(float s) {
    if (s > 20000) {
        return true;
    }
    return false;
}

void Empleado::mostrarInformacion() {
    cout << "Nombre: ";
    cout << nombre;
    cout << "\nSalario:\n ";
    cout << salario;
}

int main() {
    Empleado e1;
    Empleado e2(1, 20000);
    e1.setSalario(25000);
    e1.setNombre("Carlos");
    Gerente g1(50000, "RH");
    g1.mostrarInformacion();
}