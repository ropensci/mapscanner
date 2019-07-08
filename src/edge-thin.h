#include <vector>
#include <unordered_set>
#include <unordered_map>

#include <Rcpp.h>

// https://homepages.inf.ed.ac.uk/rbf/HIPR2/thin.htm

// # nocov start
// -- this is not used at present

struct filter {
    const std::vector <bool> f;
    const std::vector <int> x, y;
};

struct filters_thin {
    filter fa1 {{false, false, false,
                        true,
                 true,  true,  true},
           {-1, 0, 1, 0, -1, 0, 1},
           {1, 1, 1, 0, -1, -1, -1}};

    filter fa2 {{true,       false,
                 true, true, false,
                 true,       false},
           {-1, 1, -1, 0, 1, -1, 1},
           {1, 1, 0, 0, 0, -1, -1}};

    filter fa3 {{true,  true,  true,
                        true,
                 false, false, false},
           {-1, 0, 1, 0, -1, 0, 1},
           {1, 1, 1, 0, -1, -1, -1}};

    filter fa4 {{false,       true,
                 false, true, true,
                 false,       true},
           {-1, 1, -1, 0, 1, -1, 1},
           {1, 1, 0, 0, 0, -1, -1}};

    filter fb1 {{      false, false,
                 true, true,  false,
                       true},
           {0, 1, -1, 0, 1, 0},
           {1, 1, 0, 0, 0, -1}};

    filter fb2 {{      true,
                 true, true,  false,
                       false, false},
           {1, -1, 0, 1, 0, 1},
           {1, 0, 0, 0, -1, -1}};

    filter fb3 {{       true,
                 false, true, true,
                 false, false},
           {0, -1, 0, 1, -1, 0},
           {1, 0, 0, 0, -1, -1}};

    filter fb4 {{false, false,
                 false, true, true,
                        true},
           {-1, 0, -1, 0, 1, 0},
           {1, 1, 0, 0, 0, -1}};
};

// pruning filters are not used here
struct filters_prune {
    filter fa1 {{false, false, false,
                 false, true, false,
                 false},
           {-1, 0, 1, -1, 0, 1, -1},
           {1, 1, 1, 0, 0, 0, -1}};

    filter fa2 {{false, false, false,
                        true,  false,
                        false, false},
           {-1, 0, 1, 0, 1, 0, 1},
           {1, 1, 1, 0, 0, -1, -1}};

    filter fa3 {{             false,
                 false, true, false,
                 false, false, false},
           {1, -1, 0, 1, -1, 0, 1},
           {1, 0, 0, 0, -1, -1, -1}};

    filter fa4 {{false, false,
                 false, true,
                 false, false, false},
           {-1, 0, -1, 0, -1, 0, 1},
           {1, 1, 0, 0, -1, -1, -1}};

    filter fb1 {{false, false, false,
                 false, true,  false,
                               false},
           {-1, 0, 1, -1, 0, 1, 1},
           {1, 1, 1, 0, 0, 0, -1}};

    filter fb2 {{       false, false,
                        true,  false,
                 false, false, false},
           {0, 1, 0, 1, -1, 0, 1},
           {1, 1, 0, 0, -1, -1, -1}};

    filter fb3 {{false,
                 false, true,  false,
                 false, false, false},
           {-1, -1, 0, 1, -1, 0, 1},
           {1, 0, 0, 0, -1, -1, -1}};

    filter fb4 {{false, false, false,
                 false, true,
                 false, false},
           {-1, 0, 1, -1, 0, -1, 0},
           {1, 1, 1, 0, 0, -1, -1}};
};

// # nocov end

bool applyOneFilter (Rcpp::LogicalMatrix &image, filter f);

int rcpp_edge_thin (Rcpp::LogicalMatrix image);
