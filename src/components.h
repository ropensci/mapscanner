#include <vector>
#include <unordered_set>
#include <unordered_map>

#include <Rcpp.h>

struct pair_hash
{
    std::size_t operator () (std::pair<int, int> const &pair) const
    {
        std::size_t h1 = std::hash <int> () (pair.first);
        std::size_t h2 = std::hash <int> () (pair.second);

        return h1 ^ h2;
    }
};


// These are all <int>s to allow for relative offsets (-1, 0, 1):
typedef std::unordered_set <std::pair <int, int>, pair_hash > NbSet;

void traceComponent (Rcpp::LogicalMatrix &image, int i, int j,
        Rcpp::IntegerMatrix &comp_mat, int group_num);

void getNeighbours (
        Rcpp::LogicalMatrix &image, int i, int j,
        Rcpp::IntegerMatrix &comp_mat,
        NbSet &neighbours);

std::pair <int, int> getStartingMember (Rcpp::LogicalMatrix &image,
        Rcpp::IntegerMatrix &group_membership);

Rcpp::IntegerMatrix rcpp_components (Rcpp::LogicalMatrix image);
