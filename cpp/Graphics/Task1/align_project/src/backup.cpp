#include "align.h"
#include <string>
#include <algorithm>
using std::string;
using std::cout;
using std::endl;
using std::tie;
using std::make_tuple;
using std::sort;

struct Shift
{
    int y;
    int x;

    Shift(): y(0),x(0) {}
    Shift(int a, int b): y(a),x(b) {}
};

Shift MSE(Image img, uint k)
{
    int rows = img.n_rows / 3, cols = img.n_cols;
    int i, j;
    double min = 0;
    int ysh, xsh, min_ysh = 15, min_xsh = 15;
    for (i = 15; i < rows-15; i++)
        for (j = 15; j < cols-15; j++)
        {
            uint r,g,b;
            double x;
            tie(r,g,b) = img(i+rows+15,j+15);
            x = r;
            tie(r,g,b) = img(i+k*rows,j);
            x -= r;
            min += x*x/(rows*cols);
        }
    for (xsh = -15; xsh < 15; xsh++)
        for (ysh = -15; ysh < 15; ysh++)
        {
            double s = 0;
            for (i = 15; i < rows-15; i++)
            {
                for (j = 15; j < cols-15; j++)
                {
                    uint r,g,b;
                    double x;
                    tie(r,g,b) = img(i+ysh+rows,j+xsh);
                    x = r;
                    tie(r,g,b) = img(i+k*rows,j);
                    x -= r;
                    s += x*x/(rows*cols);
                }
            }
            if (s < min)
            {
                min_ysh = ysh;
                min_xsh = xsh;
                min = s;
            }
        }
    return Shift(min_ysh, min_xsh);
}

Shift CrossCorr(Image img, uint k)
{
    int rows = img.n_rows / 3, cols = img.n_cols;
    int i, j;
    double max = 0;
    int ysh, xsh, max_ysh = 0, max_xsh = 0;
    for (xsh = -15; xsh <= 15; xsh++)
        for (ysh = -15; ysh <= 15; ysh++)
        {
            double s = 0;
            for (i = 15; i < rows-15; i++)
            {
                for (j = 15; j < cols-15; j++)
                {
                    uint r,g,b;
                    uint x;
                    tie(r,g,b) = img(i+ysh+rows,j+xsh);
                    x = r;
                    tie(r,g,b) = img(i+k*rows,j);
                    x *= r;
                    s += x;
                }
            }
            if (s > max)
            {
                max_ysh = ysh;
                max_xsh = xsh;
                max = s;
            }
        }
    return Shift(max_ysh, max_xsh);
} 

Image mirror(Image src_image, int radius = 1)
{
    int i,j,rows = src_image.n_rows, cols = src_image.n_cols;
    Image new_image(2*radius + rows, 2*radius + cols);
    for (i = radius; i < rows+radius; i++)
        for (j = radius; j < cols+radius; j++)
            new_image(i,j) = src_image(i-radius, j-radius);
    for (i = 0; i < radius; i++)
        for (j = radius; j < cols+radius; j++)
            new_image(i,j) = src_image(radius-1-i, j-radius);
    for (i = 0; i < radius; i++)
        for (j = 0; j < radius; j++)
            new_image(i,j) = src_image(radius-1-i, radius-1-j);
    for (i = radius; i < rows+radius; i++)
        for (j = 0; j < radius; j++)
            new_image(i,j) = src_image(i-radius, radius-1-j);
    for (i = radius+rows; i < rows+2*radius; i++)
        for (j = 0; j < radius; j++)
            new_image(i,j) = src_image(2*rows+radius-1-i, radius-1-j);
    for (i = radius+rows; i < rows+2*radius; i++)
        for (j = radius; j < cols+radius; j++)
            new_image(i,j) = src_image(2*rows+radius-1-i, j-radius);
    for (i = radius+rows; i < rows+2*radius; i++)
        for (j = radius+cols; j < cols+2*radius; j++)
            new_image(i,j) = src_image(2*rows+radius-1-i, 2*cols+radius-1-j);
    for (i = radius; i < rows+radius; i++)
        for (j = radius+cols; j < cols+2*radius; j++)
            new_image(i,j) = src_image(i-radius, 2*cols+radius-1-j);
    for (i = 0; i < radius; i++)
        for (j = radius+cols; j < cols+2*radius; j++)
            new_image(i,j) = src_image(radius-1-i, 2*cols+radius-1-j);
    return new_image;
}

Image unmirror(Image src_image, int radius = 1)
{
    int i,j,rows = src_image.n_rows - 2*radius, cols = src_image.n_cols - 2*radius;
    Image new_image(rows, cols);
    for (i = 0; i < rows; i++)
        for (j = 0; j < cols; j++)
            new_image(i,j) = src_image(i+radius,j+radius);
    return new_image;
}

Image align(Image srcImage, bool isPostprocessing, std::string postprocessingType, double fraction, bool isMirror, 
            bool isInterp, bool isSubpixel, double subScale)
{
    int new_rows = srcImage.n_rows / 3, new_cols = srcImage.n_cols;
    Image newImage(new_rows, new_cols);
    int i,j;
    Shift sh1 = MSE(srcImage, 0);
    Shift sh2 = MSE(srcImage, 2);
    for (i = 0; i < new_rows; i++)
        for (j = 0; j < new_cols; j++)
        {
            uint r,g,b, new_r, new_g, new_b;
            if (i-sh1.y < 0 && (j-sh1.x < 0 || j-sh1.x > new_cols-1))
                tie(r,g,b) = srcImage(i,j);
            else if (j-sh1.x < 0 || j-sh1.x > new_cols-1)
                tie(r,g,b) = srcImage(i - sh1.y,j);
            else if (i-sh1.y < 0)
                tie(r,g,b) = srcImage(i,j - sh1.x);
            else
                tie(r,g,b) = srcImage(i - sh1.y,j - sh1.x);
            new_b = b;
            tie(r,g,b) = srcImage(i + new_rows,j);
            new_g = g;
            if (i-sh2.y > new_rows-1 && (j-sh2.x < 0 || j-sh2.x > new_cols-1))
                tie(r,g,b) = srcImage(i + 2*new_rows,j);
            else if (j-sh2.x < 0 || j-sh2.x > new_cols-1)
                tie(r,g,b) = srcImage(i + 2*new_rows - sh2.y,j);
            else if (i-sh2.y > new_rows-1)
                tie(r,g,b) = srcImage(i + 2*new_rows, j - sh2.x);
            else
                tie(r,g,b) = srcImage(i + 2*new_rows - sh2.y, j - sh2.x);
            new_r = r;
            newImage(i,j) = make_tuple(new_r, new_g, new_b);
        }
    if (isPostprocessing)
    {
        if (postprocessingType == "--gray-world")
            newImage = gray_world(newImage);
        else if (postprocessingType == "--unsharp")
        {
            if (isMirror)
                newImage = unsharp(newImage);
            else
                newImage = simple_unsharp(newImage);
        }
        else if (postprocessingType == "--autocontrast")
            newImage = autocontrast(newImage,fraction);
    }
    return newImage;
}

Image sobel_x(Image src_image) {
    Matrix<double> kernel = {{-1, 0, 1},
                             {-2, 0, 2},
                             {-1, 0, 1}};
    src_image = mirror(src_image);
    return unmirror(custom(src_image, kernel));
}

Image sobel_y(Image src_image) {
    Matrix<double> kernel = {{ 1,  2,  1},
                             { 0,  0,  0},
                             {-1, -2, -1}};
    src_image = mirror(src_image);
    return unmirror(custom(src_image, kernel));
}

Image simple_unsharp(Image src_image)
{
    Matrix<double> kernel = {{-1.0/6, -2.0/3, -1.0/6},
                             {-2.0/3, 13.0/3, -2.0/3},
                             {-1.0/6, -2.0/3, -1.0/6}};
    return custom(src_image,kernel);
}

Image unsharp(Image src_image) {
    Matrix<double> kernel = {{-1.0/6, -2.0/3, -1.0/6},
                             {-2.0/3, 13.0/3, -2.0/3},
                             {-1.0/6, -2.0/3, -1.0/6}};
    src_image = mirror(src_image);
    return unmirror(custom(src_image,kernel));
}

Image gray_world(Image src_image) {
    double s = 0, sr = 0, sg = 0, sb = 0;
    uint i,j;
    for (i = 0; i < src_image.n_rows; i++)
        for (j = 0; j < src_image.n_cols; j++)
        {
            uint r,g,b;
            tie(r,g,b) = src_image(i,j);
            sr += r; sg += g; sb += b;
        }
    sr /= src_image.n_rows*src_image.n_cols;
    sg /= src_image.n_rows*src_image.n_cols;
    sb /= src_image.n_rows*src_image.n_cols;
    s = sr/3 + sg/3 + sb/3;
    for (i = 0; i < src_image.n_rows; i++)
        for (j = 0; j < src_image.n_cols; j++)
        {
            uint r,g,b;
            tie(r,g,b) = src_image(i,j);
            r *= s/sr; g *= s/sg; b *= s/sb;
            if (r > 255)
                r = 255;
            if (g > 255)
                g = 255;
            if (b > 255)
                b = 255;
            src_image(i,j) = make_tuple(r,g,b);
        }
    return src_image;
}

Image resize(Image src_image, double scale) {
    int rows = round(src_image.n_rows * scale), i,j,
        cols = round(src_image.n_cols * scale);
    Image new_image(rows,cols);
    for (i = 0; i < rows; i++)
        for (j = 0; j < cols; j++)
        {
            uint r,g,b;
            tie(r,g,b) = src_image(floor(i/scale), floor(j/scale));
            new_image(i,j) = make_tuple(r,g,b);
        }
    return new_image;
}

Image custom(Image src_image, Matrix<double> kernel) {
    // Function custom is useful for making concrete linear filtrations
    // like gaussian or sobel. So, we assume that you implement custom
    // and then implement other filtrations using this function.
    // sobel_x and sobel_y are given as an example.
    int i,j, rows = src_image.n_rows, cols = src_image.n_cols,
        x,y, m = (kernel.n_rows-1)/2, n = (kernel.n_cols-1)/2;
    Image new_image = src_image.deep_copy();
    for (i = m; i < rows-m; i++)
        for (j = m; j < cols-m; j++)
        {
            double sr = 0, sg = 0, sb = 0;
            for (x = -m; x <= m; x++)
                for (y = -n; y <= n; y++)
                {
                    uint r,g,b;
                    tie(r,g,b) = src_image(i+x,j+y);
                    sr += r*kernel(m+x,n+y);
                    sg += g*kernel(m+x,n+y);
                    sb += b*kernel(m+x,n+y);
                }
            if (sr > 255)
                sr = 255;
            else if (sr < 0)
                sr = 0;
            if (sg > 255)
                sg = 255;
            else if (sg < 0)
                sg = 0;
            if (sb > 255)
                sb = 255;
            else if (sb < 0)
                sb = 0;
            new_image(i,j) = make_tuple(sr,sg,sb);
        }
    return new_image;
}

Image autocontrast(Image src_image, double fraction) {
    int H[256];
    int i,j,k, rows = src_image.n_rows, cols = src_image.n_cols;
    for (i = 0; i < 256; i++)\
        H[i] = 0;
    for (i = 0; i < rows; i++)
        for (j = 0; j < cols; j++)
        {
            uint r,g,b;
            tie(r,g,b) = src_image(i,j);
            k = round(0.2125*r + 0.7154*g + 0.0721*b);
            (H[k])++;
        }
    k = round(fraction*rows*cols);
    i = 0, j = k;
    uint max,min;
    while (j >= 0) // == 0 when fraction is 0.0 and we haven't black color in the picture
    {
        j -= H[i];
        i++;
    }
    min = (j < 0) ? (i-1) : i;
    i = 255;
    while (k >= 0) // == 0 when fraction is 0.0 and we haven't white color in the picture
    {
        k -= H[i];
        i--;
    }
    max = (k < 0) ? (i+1) : i;
    double f = 255.0/(max-min);
    Image new_image(rows,cols);
    for (i = 0; i < rows; i++)
        for (j = 0; j < cols; j++)
        {
            int r,g,b;
            tie(r,g,b) = src_image(i,j);
            r = f*(r-min); g = f*(g-min); b = f*(b-min);
            if (r > 255)
                r = 255;
            else if (r < 0)
                r = 0;
            if (g > 255)
                g = 255;
            else if (g < 0)
                g = 0;
            if (b > 255)
                b = 255;
            else if (b < 0)
                b = 0;
            new_image(i,j) = make_tuple(r,g,b);
        }
    return new_image;
}

Image gaussian(Image src_image, double sigma, int radius)  {
    return src_image;
}

Image gaussian_separable(Image src_image, double sigma, int radius) {
    return src_image;
}

Image median(Image src_image, int radius) {
    src_image = mirror(src_image,radius);
    int i,j,x,y, rows = src_image.n_rows, cols = src_image.n_cols;
    Image new_image(rows,cols);
    for (i = radius; i < rows-radius; i++)
        for (j = radius; j < cols-radius; j++)
        {
            uint ar[(2*radius+1)*(2*radius+1)],
                 ag[(2*radius+1)*(2*radius+1)],
                 ab[(2*radius+1)*(2*radius+1)];
            int k = 0;
            for (x = -radius; x <= radius; x++)
                for (y = -radius; y <= radius; y++)
                {
                    uint r,g,b;
                    tie(r,g,b) = src_image(i+x,j+y);
                    ar[k] = r; ag[k] = g; ab[k] = b;
                    k++;
                }
            sort(ar,ar + (2*radius+1)*(2*radius+1));
            sort(ag,ag + (2*radius+1)*(2*radius+1));
            sort(ab,ab + (2*radius+1)*(2*radius+1));
            new_image(i,j) = make_tuple(ar[((2*radius+1)*(2*radius+1) - 1)/2],
                                        ag[((2*radius+1)*(2*radius+1) - 1)/2],
                                        ab[((2*radius+1)*(2*radius+1) - 1)/2]);
        }
    return unmirror(new_image,radius);
}

void inc(uint H[], int radius)
{
    int i,j;
    for (i = 0; i < 2*radius+1; i++)
        for (j = 1; j < 2*radius+1; j++)
            H[i*(2*radius+1) + j-1] = H[i*(2*radius+1) + j];
    return;
}

uint med(uint H[], int radius)
{
    int i,j,k = 2*radius+1;
    uint a[k*k];
    for (i = 0; i < k; i++)
        for (j = 0; j < k; j++)
            a[i*k + j] = H[i*k + j];
    sort(a, a+k*k);
    return a[(k*k - 1)/2];
}

Image median_linear(Image src_image, int radius) {
    src_image = mirror(src_image,radius);
    int i,j,x,y, rows = src_image.n_rows, cols = src_image.n_cols,
    k = 2*radius+1;
    Image new_image(rows,cols);
    uint Ar[k * k],
         Ag[k * k],
         Ab[k * k];
    for (i = radius; i < rows-radius; i++)
    {
        for (x = 0; x < k; x++)
            for (y = 0; y < k; y++)
            {
               	uint r,g,b;
                tie(r,g,b) = src_image(i+x-radius,y);
                Ar[x*k + y] = r; Ag[x*k + y] = g; Ab[x*k + y] = b;
            }
        new_image(i,radius) = make_tuple(med(Ar,radius),
                                         med(Ag,radius),
                                         med(Ab,radius));
        for (j = radius+1; j < cols-radius; j++)
        {
            inc(Ar,radius); inc(Ag,radius); inc(Ab,radius);
            for (x = -radius; x <= radius; x++)
            {
                
                uint r,g,b;
                tie(r,g,b) = src_image(i+x,j+radius);
                Ar[(radius+x)*k + k-1] = r;
                Ag[(radius+x)*k + k-1] = g;
                Ab[(radius+x)*k + k-1] = b;
            }
            new_image(i,j) = make_tuple(med(Ar,radius),
                                        med(Ag,radius),
                                        med(Ab,radius));
        }
    }
    return unmirror(new_image,radius);
}

Image median_const(Image src_image, int radius) {
    return src_image;
}

Image canny(Image src_image, int threshold1, int threshold2) {
    return src_image;
}
