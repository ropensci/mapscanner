#include "edge-thin.h"


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
        for (int j = 1; j < (image.ncol () - 1); j++)
        {
            if (image (i, j)) // NOTE: Does not work with "[i, j]"!!
            {
                bool match = true;
                for (int k = 0; k < f.f.size (); k++)
                {
                    if (image (i + f.x [k], j + f.y [k]) != f.f [k])
                        match = false;
                }
                if (match)
                {
                    image (i, j) = false;
                    changed = true;
                }
            }
        }

    return changed;
}
