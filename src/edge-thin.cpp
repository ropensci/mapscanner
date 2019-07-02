#include "edge-thin.h"


//' rcpp_edge_thin
//' @noRd 
// [[Rcpp::export]]
void rcpp_edge_thin (Rcpp::LogicalMatrix image)
{
    struct filters_thin fthin;

    bool changed = true;
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
    }
}

bool applyOneFilter (Rcpp::LogicalMatrix &image, filter f)
{
    const int nx = image.ncol (), ny = image.nrow ();
    bool changed = false;

    for (int i = 1; i < (nx - 1); i++)
        for (int j = 1; j < (ny - 1); j++)
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
