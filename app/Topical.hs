{-# LANGUAGE OverloadedStrings #-}


module Main where


-- training:
-- 1. Determine vocabulary;
-- 2. Initialize (OnlineLDA.__init__)
-- 3. train on a batch of documents
--    a. `update_lambda`
--    b. returns (gamma, bound)
-- 4. estimate held-out perplexity
--    a. `parse_doc_list`
--    b. batch of documents,
--    c. vocabulary,
--    d. returns (word ids, word counts)
-- 5. track/save
--    a. lambda, the parameters to the variational distributions over topics;
--    b. gamma, the parameters to the variational distributions over topic
--       weights for the articles analyzed in the last iteration;

-- dirichlet_expectation
-- alpha -> E[log(Dir(alpha))]

-- parse_doc_list
-- [Document] -> Vocab -> [[(WordId, WordCount)]]

-- OnlineLDA from Hoffman, et al., 2010

-- OnlineLDA.__init__
--    a. vocabulary,
--    b. topic count (K),
--    c. number of documents in population (D),
--    d. alpha (1/K),
--    e. eta (1/K),
--    f. tau0 (1024),
--    g. kappa (0.7);
-- _vocab :: Map Token WordId
-- _W = size _vocab
-- _tau0 = tau0 + 1
-- _updatect = 0
-- _lambda = random matrix
-- _Elogbeta = dirichlet_expectation(_lambda)
-- _expElogbeta = exp(_Elogbeta)

-- OnlineLDA.do_e_step
-- [Document] -> (Map DocumentId GammaMatrix, ExpectedSufficientStats)
-- estimates the parameters gamma controlling the variational distribution
-- over the topic weights for each document input

-- OnlineLDA.update_lambda
-- [Document] -> (Map DocumentId GammaMatrix, OldLambda)
-- Performs E steps on mini-batches and updates variational parameter matrix
-- lambda

-- OnlineLDA.approx_bound
-- [Document] -> Map DocumentId GammaMatrix -> Double
-- Estimates the variational bound over all documents using the input subset.
-- Output is noisy, but can be used to assess convergence.

-- printtopics
-- 1. read vocabulary;
-- 2. read lambdas;
-- 3. lambda_word = vector of lambdas over topics for a word;
-- 4. lambdak = lambda_word / sum(lambda_word);
-- 5. sort and print

main :: IO ()
main = undefined
