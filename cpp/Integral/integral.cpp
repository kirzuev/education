#include <iostream>
#include <cmath>
using namespace std;

const double f1(const double x) { return exp(-x*x); }
const double f2(const double x) { return sin(x*x); }
const double f3(const double x) { return cos(x)/x; }
const double f4(const double x) { return 1/log(x); }
const double f5(const double x) { return exp(x)/x; }

class Integral
{
	protected:
		double a,b;
		int n;
	public:
		Integral(const int k = 100, const double x = 0, const double y = 1)
		{
			if (x < y) { a = x; b = y; }
			else { a = y; b = x; }
			n = k;
		}
		virtual const double integrate(const double f(const double)) = 0;
};

//метод прямоугольников
class Rectangle: virtual public Integral
{
	public:
		Rectangle(const int k = 100, const double x = 0, const double y = 1)
		{
			if (x < y) { a = x; b = y; }
			else { a = y; b = x; }
			n = k;
		}

		const double integrate(const double f(const double))
		{
			const double h = (b-a)/n;
			double x = a+h/2, sum = 0;
			int i;
			for (i = 0; i < n; i++)
			{
				sum += h*f(x);
				x += h;
			}
			return sum;
		}
};

//метод трапеций
class Trapeze: virtual public Integral
{
	public:
		Trapeze(const int k = 100, const double x = 0, const double y = 1)
		{
			if (x < y) { a = x; b = y; }
			else { a = y; b = x; }
			n = k;
		}

		const double integrate(const double f(const double))
		{
			const double h = (b-a)/n;
			double x = a, y = a+h, sum = 0;
			int i;
			for (i = 0; i < n; i++)
			{
				sum += h*(f(x) + f(y))/2;
				x = y;
				y += h;
			}
			return sum;
		}
};

//метод парабол(Симпсона)
class Simpson: virtual public Integral
{
	public:
		Simpson(const int k = 100, const double x = 0, const double y = 1)
		{
			if (x < y) { a = x; b = y; }
			else { a = y; b = x; }
			n = k;
		}

		const double integrate(const double f(const double))
		{
			const double h = (b-a)/(2*n);
			int i;
			double x, sum = h*(f(a) + f(b))/3;
			for (i = 1, x = a+h; i < n+1; i++)
			{
				sum += 4*h*f(x)/3;
				x += 2*h;
			}
			for (i = 1, x = a+2*h; i < n; i++)
			{
				sum += 2*h*f(x)/3;
				x += 2*h;
			}
			return sum;
		}
};

class RecTra: public Rectangle, public Trapeze
{
	public:
		RecTra(const int k = 100, const double x = 0, const double y = 1)
		{
			if (x < y) { a = x; b = y; }
			else { a = y; b = x; }
			n = k;
		}

		const double integrate(const double f(const double))
		{
			return (Rectangle::integrate(f) + Trapeze::integrate(f))/2;
		}
};

class RecSimp: public Rectangle, public Simpson
{
	public:
		RecSimp(const int k = 100, const double x = 0, const double y = 1)
		{
			if (x < y) { a = x; b = y; }
			else { a = y; b = x; }
			n = k;
		}

		const double integrate(const double f(const double))
		{
			return (Rectangle::integrate(f) + Simpson::integrate(f))/2;
		}
};

class TraSimp: public Trapeze, public Simpson
{
	public:
		TraSimp(const int k = 100, const double x = 0, const double y = 1)
		{
			if (x < y) { a = x; b = y; }
			else { a = y; b = x; }
			n = k;
		}

		const double integrate(const double f(const double))
		{
			return (Trapeze::integrate(f) + Simpson::integrate(f))/2;
		}
};

class RecTraSimp: public Rectangle, public Trapeze, public Simpson
{
	public:
		RecTraSimp(const int k = 100, const double x = 0, const double y = 1)
		{
			if (x < y) { a = x; b = y; }
			else { a = y; b = x; }
			n = k;
		}

		const double integrate(const double f(const double))
		{
			return (Rectangle::integrate(f) + Trapeze::integrate(f) + Simpson::integrate(f))/3;
		}
};

void calculate(Integral* arr[], int k)
{
	int i;
	for (i = 0; i < k; i++)
	{
		cout << "===============================" << endl;
		cout << "* I(e^(-x^2)) = " << (arr[i])->integrate(f1) << endl;
		cout << "* I(sin(x^2)) = " << (arr[i])->integrate(f2) << endl;
		cout << "* I(cos(x)/x) = " << (arr[i])->integrate(f3) << endl;
		cout << "* I(1/ln(x)) = " << (arr[i])->integrate(f4) << endl;
		cout << "* I(e^x/x) = " << (arr[i])->integrate(f5) << endl;
		cout << "===============================" << endl;
	}
	return;
}

int main()
{
	Integral* arr[10];
	arr[0] = new Simpson(20, 1.89, 10.09);
	arr[1] = new Rectangle(150, 10, 50);
	arr[2] = new Trapeze(200, 4, 20);
	arr[3] = new TraSimp(100, 40, 10);
	arr[4] = new RecTra(400, 90, 10);
	arr[5] = new RecSimp(80, 1.23, 0.9);
	arr[6] = new Simpson(30, 2, 9);
	arr[7] = new RecTraSimp(50, 1.09, 2.87);
	arr[8] = new Trapeze(150, 2, 9);
	arr[9] = new Simpson(20, 0.1, 0.11);
	calculate(arr,10);
	return 0;
}
