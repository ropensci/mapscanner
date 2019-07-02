#include "components.h"

//' rcpp_components
//' @noRd 
// [[Rcpp::export]]
Rcpp::IntegerMatrix rcpp_components (Rcpp::LogicalMatrix image)
{
    Rcpp::IntegerMatrix comp_mat (image.nrow (), image.ncol ());

    int group_num = 1;
    std::pair <int, int> start_ij = getStartingMember (image, comp_mat);

    while (start_ij.first >= 0 && start_ij.second >= 0)
    {
        traceComponent (image, start_ij.first, start_ij.second,
                comp_mat, group_num++);
        start_ij = getStartingMember (image, comp_mat);
    }

    return comp_mat;
}

std::pair <int, int> getStartingMember (Rcpp::LogicalMatrix &image,
        Rcpp::IntegerMatrix &comp_mat)
{
    int member_i = -1, member_j = -1;
    bool found = false;
    for (int i = 1; i < (image.nrow () - 1); i++)
    {
        for (int j = 1; j < (image.ncol () - 1); j++)
        {
            if (image (i, j) && comp_mat (i, j) == 0)
            {
                member_i = i;
                member_j = j;
                found = true;
                break;
            }
        }
        if (found)
            break;
    }
    std::pair <int, int> result = std::make_pair (member_i, member_j);
    return result;
}

void traceComponent (Rcpp::LogicalMatrix &image, int i, int j,
        Rcpp::IntegerMatrix &comp_mat, int group_num)
{
    comp_mat (i, j) = group_num;
    std::unordered_set <std::pair <int, int>, pair_hash> nbs_to_do;
    getNeighbours (image, i, j, comp_mat, nbs_to_do);

    while (nbs_to_do.size () > 0)
    {
        std::pair <int, int> n = (*nbs_to_do.begin ());
        comp_mat (n.first, n.second) = group_num;
        nbs_to_do.erase (n);
        getNeighbours (image, n.first, n.second, comp_mat, nbs_to_do);
    }
}

void getNeighbours (
        Rcpp::LogicalMatrix &image, int i, int j,
        Rcpp::IntegerMatrix &comp_mat,
        NbSet &neighbours)
{
    std::vector <int> dx {-1, 0, 1, -1, 1, -1, 0, 1},
        dy {1, 1, 1, 0, 0, -1, -1, -1};

    for (int k = 0; k < dx.size (); k++)
    {
        if (image (i + dx [k], j + dy [k]) &&
                comp_mat (i + dx [k], j + dy [k]) == 0)
        {
            std::pair <int, int> nbPair = std::make_pair (i + dx [k], j + dy [k]);
            neighbours.emplace (nbPair);
        }
    }
}
