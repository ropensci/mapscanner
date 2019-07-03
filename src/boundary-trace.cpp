#include "boundary-trace.h"

// mostly from https://en.wikipedia.org/wiki/Moore_neighborhood#Algorithm

//' rcpp_components
//' @noRd 
// [[Rcpp::export]]
Rcpp::DataFrame rcpp_boundary (Rcpp::LogicalMatrix image)
{
    int startj = 1;
    int starti = findStartPixel (image, startj);

    std::vector <int> bx, by;
    bx.push_back (starti);
    by.push_back (startj);

    XYPoint s = std::make_pair (starti, startj);

    XYPoint p;
    p.first = starti;
    p.second = startj;

    // Entry point is always same:
    XYPoint b = std::make_pair (starti - 1, startj);
    XYPoint c = nextClockwise (b, s);

    // p is always the current - central - boundary point = p_mid in
    // nextClockwise
    while (c != s)
    {
        if (image (c.first, c.second))
        {
            bx.push_back (c.first);
            by.push_back (c.second);
            b = p;
            p = c;
            c = nextClockwise (b, p);
        } else
        {
            b = c;
            c = nextClockwise (b, p);
        }
    }

    Rcpp::DataFrame res = Rcpp::DataFrame::create (
            Rcpp::Named ("x") = bx,
            Rcpp::Named ("y") = by);
    return res;
}

int findStartPixel (Rcpp::LogicalMatrix image, int startj)
{
    int starti = -1;
    for (int i = 0; i < image.nrow (); i++)
        if (image (i, startj))
        {
            starti = i;
            break;
        }

    if (starti < 0)
        Rcpp::stop ("No start pixel found");

    return starti;
}

XYPoint nextClockwise (XYPoint p_in, XYPoint p_mid)
{
    // start from top-right:
    std::vector <int> x {-1, 0, 1, 1, 1, 0, -1, -1},
        y {1, 1, 1, 0, -1, -1, -1, 0};

    XYPoint p_diff = std::make_pair (p_in.first - p_mid.first,
            p_in.second - p_mid.second);

    int p_i = -1;
    for (int i = 0; i < x.size (); i++)
        if (x [i] == p_diff.first && y [i] == p_diff.second)
        {
            p_i = i;
            break;
        }
    if (p_i < 0)
        Rcpp::stop ("Nope");

    p_i++;
    if (p_i == x.size ())
        p_i = 0;

    XYPoint p = std::make_pair (p_mid.first + x [p_i], p_mid.second + y [p_i]);

    return p;
}
