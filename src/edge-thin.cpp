#include "edge-thin.h"

// # nocov start

// https://homepages.inf.ed.ac.uk/rbf/HIPR2/thin.htm

//' rcpp_edge_thin
//' @noRd 
// [[Rcpp::export]]
int rcpp_edge_thin (Rcpp::LogicalMatrix image)
{
    struct filters_thin fthin;

    bool changed = true;
    int count = 0;
    while (changed)
    {
        changed = applyOneFilter (image, fthin.fa1);
        changed = changed || applyOneFilter (image, fthin.fb1);
        changed = changed || applyOneFilter (image, fthin.fa2);
        changed = changed || applyOneFilter (image, fthin.fb2);
        changed = changed || applyOneFilter (image, fthin.fa3);
        changed = changed || applyOneFilter (image, fthin.fb3);
        changed = changed || applyOneFilter (image, fthin.fa4);
        changed = changed || applyOneFilter (image, fthin.fb4);
        count++;
    }

    return count;
}

bool applyOneFilter (Rcpp::LogicalMatrix &image, filter f)
{
    bool changed = false;

    for (int i = 1; i < (image.nrow () - 1); i++)
    {
        size_t i_t = static_cast <size_t> (i);
        for (int j = 1; j < (image.ncol () - 1); j++)
        {
            size_t j_t = static_cast <size_t> (j);
            // NOTE: Does not work with "image [i, j]"!!
            if (image (i_t, j_t))
            {
                bool match = true;
                for (size_t k = 0; k < f.f.size (); k++)
                {
                    if (image (static_cast <size_t> (i + f.x [k]),
                                static_cast <size_t> (j + f.y [k])) != f.f [k])
                        match = false;
                }
                if (match)
                {
                    image (i_t, j_t) = false;
                    changed = true;
                }
            }
        }
    }

    return changed;
}

// # nocov end
