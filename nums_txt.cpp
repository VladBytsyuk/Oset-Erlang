#include <iostream>
#include <fstream>
#include <cstdlib>
#include <time.h>
#include <set>
#include <string>

using namespace std;

int NUMBERS_AMOUNT = 0;

int fill_file() {
	double start_time;
	double end_time;
	double result_time;
	
	fstream file_stream;
	file_stream.open("numbers.txt", ios::out);
	
	start_time = clock();
	for (int i = 0; i < NUMBERS_AMOUNT; ++i) {
		int buf = rand();// % 10000;
		file_stream << buf << endl;
	}
	end_time = clock();
	result_time = end_time - start_time;
	
	file_stream.close();
	cout << "	Filling file time: " << result_time / CLOCKS_PER_SEC  << "s." << endl;
	
	file_stream.open("numbers_2.txt", ios::out);
	for (int i = 0; i < NUMBERS_AMOUNT; ++i) {
		int buf = rand();// % 10000;
		file_stream << buf << endl;
	}
	file_stream.close();
	
	return 0;
}

set<int> fill_set() {
	set<int> my_set;
	int buf = 0;
	
	double start_time;
	double end_time;
	double result_time;

	fstream file_stream;
	file_stream.open("numbers.txt", ios::in);
	
	start_time = clock();
	for (int i = 0; i < NUMBERS_AMOUNT; ++i) {
		//if (i % (NUMBERS_AMOUNT / 100) == 0) cout << "	Loading " << i / (NUMBERS_AMOUNT / 100) << "%..." << endl;
		file_stream >> buf;
		my_set.insert(buf);
	}
	end_time = clock();
	result_time = end_time - start_time;
	file_stream.close();
	cout << "	Filling set time: " << result_time / CLOCKS_PER_SEC << "s." << endl;
	return my_set;
}

int set_length(set<int> my_set) {
	double start_time;
	double end_time;
	double result_time;
	
	start_time = clock();
	int length = my_set.size();
	end_time = clock();
	result_time = end_time - start_time;
	
	cout << "		Length: " << length << " (" << result_time / CLOCKS_PER_SEC << "s.)" << endl;
	return 0;
	
}

int is_elem(set<int> my_set) {
	double start_time;
	double end_time;
	double result_time;
	
	start_time = clock();
	for (int i = 0; i < 1000; ++i) {
		/*bool is_el =*/ my_set.find(2040710597) != my_set.end();
	}
	end_time = clock();
	result_time = end_time - start_time;
	
	/*
	string tr = "true";
	string fal = "false";
	string res = (is_el == 1 ? tr : fal);
	*/
	cout << "		Is element: " << /*res*/"Done" << " (" << result_time / CLOCKS_PER_SEC / 1000 << "s.)" << endl;
	return 0;
}

int remove_set(set<int> my_set) {
	double start_time;
	double end_time;
	double result_time;
	
	start_time = clock();
	for (int i = 0; i < my_set.size(); ++i)
		my_set.erase(i);
	end_time = clock();
	result_time = end_time - start_time;
	
	cout << "	Remove set: " << result_time / CLOCKS_PER_SEC  << "s." << endl;
	return 0;
}

int main(int argc, char* argv[]) {
	NUMBERS_AMOUNT = atoi(argv[1]);
	srand(time(NULL));
	
	fill_file();
	set<int> my_set = fill_set();
	set_length(my_set);
	is_elem(my_set);
	remove_set(my_set); 
	
	return 0;
}
