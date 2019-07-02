#include <vector>
#include <unordered_set>
#include <unordered_map>

#include <Rcpp.h>

struct pair_hash {
    inline std::size_t operator () (const std::pair <int, int> & v) const {
        return v.first * 31 + v.second;
    }
};

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
