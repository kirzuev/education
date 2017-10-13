#define _USE_MATH_DEFINES

#include <string>
#include <vector>
#include <fstream>
#include <cassert>
#include <iostream>
#include <cmath>

#include "classifier.h"
#include "EasyBMP.h"
#include "linear.h"
#include "argvparser.h"
#include "matrix.h"

using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;
using std::pair;
using std::make_pair;
using std::cout;
using std::cerr;
using std::endl;

using CommandLineProcessing::ArgvParser;

typedef vector<pair<BMP*, int> > TDataSet;
typedef vector<pair<string, int> > TFileList;
typedef vector<pair<vector<float>, int> > TFeatures;

// Load list of files and its labels from 'data_file' and
// stores it in 'file_list'
void LoadFileList(const string& data_file, TFileList* file_list) {
    ifstream stream(data_file.c_str());

    string filename;
    int label;
    
    int char_idx = data_file.size() - 1;
    for (; char_idx >= 0; --char_idx)
        if (data_file[char_idx] == '/' || data_file[char_idx] == '\\')
            break;
    string data_path = data_file.substr(0,char_idx+1);
    
    while(!stream.eof() && !stream.fail()) {
        stream >> filename >> label;
        if (filename.size())
            file_list->push_back(make_pair(data_path + filename, label));
    }

    stream.close();
}

// Load images by list of files 'file_list' and store them in 'data_set'
void LoadImages(const TFileList& file_list, TDataSet* data_set) {
    for (size_t img_idx = 0; img_idx < file_list.size(); ++img_idx) {
            // Create image
        BMP* image = new BMP();
            // Read image from file
        image->ReadFromFile(file_list[img_idx].first.c_str());
            // Add image and it's label to dataset
        data_set->push_back(make_pair(image, file_list[img_idx].second));
    }
}

// Save result of prediction to file
void SavePredictions(const TFileList& file_list,
                     const TLabels& labels, 
                     const string& prediction_file) {
        // Check that list of files and list of labels has equal size 
    assert(file_list.size() == labels.size());
        // Open 'prediction_file' for writing
    ofstream stream(prediction_file.c_str());

        // Write file names and labels to stream
    for (size_t image_idx = 0; image_idx < file_list.size(); ++image_idx)
        stream << file_list[image_idx].first << " " << labels[image_idx] << endl;
    stream.close();
}

// Exatract features from dataset.
// You should implement this function by yourself =)
void ExtractFeatures(const TDataSet& data_set, TFeatures* features) {
    for (size_t image_idx = 0; image_idx < data_set.size(); ++image_idx) {
        
        // PLACE YOUR CODE HERE

        vector<float> one_image_features;
		BMP input = *((data_set[image_idx]).first);
		BMP image;
		image.SetSize(32,32);
		int i,j, label = (data_set[image_idx]).second,
//		width = image.TellWidth(),
//		height = image.TellHeight();
		width = 32,
		height = 32;

		// resize
		for (i = 0; i < height; i++)
			for (j = 0; j < width; j++)
			{
				int x = i*(input.TellHeight()/height),
				y = j*(input.TellWidth()/width);
				image.SetPixel(j,i,input.GetPixel(y,x));
			}

		Matrix<float> grayscale(height,width);
		for (i = 0; i < height; i++)
			for (j = 0; j < width; j++)
			{
				// change the (i,j) to (j,i)
				RGBApixel pixel = image.GetPixel(j,i); 
				float y = 0.299*pixel.Red + 0.587*pixel.Blue + 0.114*pixel.Green;
				grayscale(i,j) = y;
			}

		Matrix<float> sobel_x(height,width);
		for (i = 0; i < height; i++)
			for (j = 1; j < width-1; j++)
				sobel_x(i,j) = -grayscale(i,j-1) + grayscale(i,j+1);
		for (i = 0; i < height; i++)
		{
			sobel_x(i,0) = grayscale(i,0);
			sobel_x(i,width-1) = grayscale(i,width-1);
		}
		
		Matrix<float> sobel_y(height,width);
		for (i = 1; i < height-1; i++)
			for (j = 0; j < width; j++)
				sobel_y(i,j) = -grayscale(i+1,j) + grayscale(i-1,j);
		for (j = 0; j < width; j++)
		{
			sobel_y(0,j) = grayscale(0,j);
			sobel_y(height-1,j) = grayscale(height-1,j);
		}

		Matrix<float> grad_abs(height,width), grad_deg(height,width);
		for (i = 0; i < height; i++)
			for(j = 0; j < width; j++)
			{
				grad_abs(i,j) = sqrt(sobel_x(i,j)*sobel_x(i,j) + 
									 sobel_y(i,j)*sobel_y(i,j));
				grad_deg(i,j) = atan2(sobel_y(i,j),sobel_x(i,j));
			}

		// HOG
		int rows = 4, cols = 4;
		float segment = 2.0*M_PI/16.0;
		for (i = 0; i <= height-rows; i += rows)
			for (j = 0; j <= width-cols; j+= cols)
			{
				vector<float> histogram(16,0.0);
				int m,n;
				for (m = 0; m < rows; m++)
					for (n = 0; n < cols; n++)
						histogram[round((grad_deg(i+m,j+n) + M_PI)/segment)] += 
							grad_abs(i+m,j+n);
			
				float norm = 0.0;
				for (n = 0; n < 16; n++)
					norm += histogram[n]*histogram[n];
				
				if (norm > 0)
				{
					norm = sqrt(norm);
					for (n = 0; n < 16; n++)
						histogram[n] /= norm;
				}
				
				one_image_features.insert(one_image_features.end(),
										  histogram.begin(), 
										  histogram.end());
			}

		// Local Binary Patterns
		rows = 5, cols = 5;
		for (i = 1; i < height-rows; i += rows)
			for ( j = 1; j < width-cols; j+= cols)
			{
				vector<float> lbp(256,0.0);
				int m,n;				
				for (m = 0; m < rows; m++)
					for (n = 0; n < cols; n++)
					{
						int num = 0;
						if (grad_abs(i+m,j+n) <= grad_abs(i+m-1,j+n-1))
							num++;
						num *= 2;
						if (grad_abs(i+m,j+n) <= grad_abs(i+m-1,j+n))
							num++;
						num *= 2;
						if (grad_abs(i+m,j+n) <= grad_abs(i+m-1,j+n+1))
							num++;
						num *= 2;
						if (grad_abs(i+m,j+n) <= grad_abs(i+m,j+n+1))
							num++;
						num *= 2;
						if (grad_abs(i+m,j+n) <= grad_abs(i+m+1,j+n+1))
							num++;
						num *= 2;
						if (grad_abs(i+m,j+n) <= grad_abs(i+m+1,j+n))
							num++;
						num *= 2;
						if (grad_abs(i+m,j+n) <= grad_abs(i+m+1,j+n-1))
							num++;
						num *= 2;
						if (grad_abs(i+m,j+n) <= grad_abs(i+m,j+n-1))
							num++;
						lbp[num] += 1;
					}

				float norm = 0.0;
				for (n = 0; n < 256; n++)
					norm += lbp[n]*lbp[n];
				
				if (norm > 0)
				{
					norm = sqrt(norm);
					for (n = 0; n < 256; n++)
						lbp[n] /= norm;
				}
				
				one_image_features.insert(one_image_features.end(), 
										  lbp.begin(), 
										  lbp.end());
			}


		// Color features
		vector<float> colors;
		rows = height/8, cols = width/8;		
		for (i = 0; i <= height-rows; i += rows)
			for (j = 0; j <= width-cols; j += cols)
			{
				float Rcolor = 0.0, Gcolor = 0.0, Bcolor = 0.0;
				int m,n;
				for (m = 0; m < rows; m++)
					for (n = 0; n < cols; n++)
					{
						RGBApixel pixel = image.GetPixel(j+n,i+m);
						Rcolor += pixel.Red;
						Gcolor += pixel.Green;
						Bcolor += pixel.Blue;
					}
				
				Rcolor /= rows*cols;
				Gcolor /= rows*cols;
				Bcolor /= rows*cols;
				Rcolor /= 255.0;
				Gcolor /= 255.0;
				Bcolor /= 255.0;
				colors.push_back(Rcolor); 
				colors.push_back(Gcolor); 
				colors.push_back(Bcolor); 
			}
		one_image_features.insert(one_image_features.end(),
								  colors.begin(),
								  colors.end());

		features->push_back(make_pair(one_image_features,label));

		// Remove this sample code and place your feature extraction code here
        /*	vector<float> one_image_features;
        	one_image_features.push_back(1.0);
        	features->push_back(make_pair(one_image_features, 1));*/
        // End of sample code

    }
}

// Clear dataset structure
void ClearDataset(TDataSet* data_set) {
        // Delete all images from dataset
    for (size_t image_idx = 0; image_idx < data_set->size(); ++image_idx)
        delete (*data_set)[image_idx].first;
        // Clear dataset
    data_set->clear();
}

// Train SVM classifier using data from 'data_file' and save trained model
// to 'model_file'
void TrainClassifier(const string& data_file, const string& model_file) {
        // List of image file names and its labels
    TFileList file_list;
        // Structure of images and its labels
    TDataSet data_set;
        // Structure of features of images and its labels
    TFeatures features;
        // Model which would be trained
    TModel model;
        // Parameters of classifier
    TClassifierParams params;
    
        // Load list of image file names and its labels
    LoadFileList(data_file, &file_list);
        // Load images
    LoadImages(file_list, &data_set);
        // Extract features from images
    ExtractFeatures(data_set, &features);

        // PLACE YOUR CODE HERE
        // You can change parameters of classifier here
    params.C = 0.01;
    TClassifier classifier(params);
        // Train classifier
    classifier.Train(features, &model);
        // Save model to file
    model.Save(model_file);
        // Clear dataset structure
    ClearDataset(&data_set);
}

// Predict data from 'data_file' using model from 'model_file' and
// save predictions to 'prediction_file'
void PredictData(const string& data_file,
                 const string& model_file,
                 const string& prediction_file) {
        // List of image file names and its labels
    TFileList file_list;
        // Structure of images and its labels
    TDataSet data_set;
        // Structure of features of images and its labels
    TFeatures features;
        // List of image labels
    TLabels labels;

        // Load list of image file names and its labels
    LoadFileList(data_file, &file_list);
        // Load images
    LoadImages(file_list, &data_set);
        // Extract features from images
    ExtractFeatures(data_set, &features);

        // Classifier 
    TClassifier classifier = TClassifier(TClassifierParams());
        // Trained model
    TModel model;
        // Load model from file
    model.Load(model_file);
        // Predict images by its features using 'model' and store predictions
        // to 'labels'
    classifier.Predict(features, model, &labels);

        // Save predictions
    SavePredictions(file_list, labels, prediction_file);
        // Clear dataset structure
    ClearDataset(&data_set);
}

int main(int argc, char** argv) {
    // Command line options parser
    ArgvParser cmd;
        // Description of program
    cmd.setIntroductoryDescription("Machine graphics course, task 2. CMC MSU, 2014.");
        // Add help option
    cmd.setHelpOption("h", "help", "Print this help message");
        // Add other options
    cmd.defineOption("data_set", "File with dataset",
        ArgvParser::OptionRequiresValue | ArgvParser::OptionRequired);
    cmd.defineOption("model", "Path to file to save or load model",
        ArgvParser::OptionRequiresValue | ArgvParser::OptionRequired);
    cmd.defineOption("predicted_labels", "Path to file to save prediction results",
        ArgvParser::OptionRequiresValue);
    cmd.defineOption("train", "Train classifier");
    cmd.defineOption("predict", "Predict dataset");
        
        // Add options aliases
    cmd.defineOptionAlternative("data_set", "d");
    cmd.defineOptionAlternative("model", "m");
    cmd.defineOptionAlternative("predicted_labels", "l");
    cmd.defineOptionAlternative("train", "t");
    cmd.defineOptionAlternative("predict", "p");

        // Parse options
    int result = cmd.parse(argc, argv);

        // Check for errors or help option
    if (result) {
        cout << cmd.parseErrorDescription(result) << endl;
        return result;
    }

        // Get values 
    string data_file = cmd.optionValue("data_set");
    string model_file = cmd.optionValue("model");
    bool train = cmd.foundOption("train");
    bool predict = cmd.foundOption("predict");

        // If we need to train classifier
    if (train)
        TrainClassifier(data_file, model_file);
        // If we need to predict data
    if (predict) {
            // You must declare file to save images
        if (!cmd.foundOption("predicted_labels")) {
            cerr << "Error! Option --predicted_labels not found!" << endl;
            return 1;
        }
            // File to save predictions
        string prediction_file = cmd.optionValue("predicted_labels");
            // Predict data
        PredictData(data_file, model_file, prediction_file);
    }
}
