library(dplyr)
library(polycor)
library(ggplot2)

f <- function(df, col_name, test_name=NULL){
    
    # 先選出所需的df
    if (col_name == "代名詞加總" & ! col_name %in% colnames(df)) {
        r <- df %>% 
            dplyr::mutate(代名詞加總=代名詞1s + 代名詞2s + 代名詞3s + 代名詞1p + 代名詞2p + 代名詞3p) %>% 
            dplyr::select(高影響力, col_name)
    } else if (col_name == "代名詞1") {
        n <- as.numeric(substr(col_name, 4, 5))
        pron_1s_str <- paste("代名詞", n, "s", sep="") 
        pron_1p_str <- paste("代名詞", n, "p", sep="") 
        r <- df %>% 
            dplyr::mutate(代名詞1 = 代名詞1s + 代名詞1p) %>% 
            dplyr::select(高影響力, col_name)
        
    } else if (col_name == "代名詞2") {
        n <- as.numeric(substr(col_name, 4, 5))
        pron_1s_str <- paste("代名詞", n, "s", sep="") 
        pron_1p_str <- paste("代名詞", n, "p", sep="") 
        r <- df %>% 
            dplyr::mutate(代名詞2 = 代名詞2s + 代名詞2p) %>% 
            dplyr::select(高影響力, col_name)
        
    }else if (col_name == "代名詞3") {
        n <- as.numeric(substr(col_name, 4, 5))
        pron_1s_str <- paste("代名詞", n, "s", sep="") 
        pron_1p_str <- paste("代名詞", n, "p", sep="") 
        r <- df %>% 
            dplyr::mutate(代名詞3 = 代名詞3s + 代名詞3p) %>% 
            dplyr::select(高影響力, col_name)
        
    }else {
        r <- df %>% dplyr::select(高影響力, col_name)
        
    } 
    
    
    # filtering test
    if (test_name == "t-test") {
        high_popular <- r %>% filter(高影響力) %>% dplyr::select(col_name)
        non_popular <- r %>% filter(!高影響力) %>% dplyr::select(col_name)
        result <- t.test(high_popular, non_popular)
    } else {

        binary_variable <- r %>% dplyr::select(高影響力)
        continuous_variable <- r %>% dplyr::select(col_name)
        result <- polycor::polyserial(unlist(continuous_variable), unlist(binary_variable))
    }
    
    return(list(df=r, test_result=result))
}

# print(f(xiaosheng_1_, "隱喻詞加總", "t-test"))
# print(f(xiaosheng_1_, "隱喻詞加總", "biserial correlation"))
# 
# print(f(xiaosheng_1_, "連接詞加總", "t-test"))
# print(f(xiaosheng_1_, "連接詞加總", "biserial correlation"))
# 
# print(f(xiaosheng_1_, "代名詞加總", "t-test"))
# print(f(xiaosheng_1_, "代名詞加總", "biserial correlation"))
# 
# print(f(xiaosheng_1_, "代名詞1", "t-test"))
# 
# print(f(xiaosheng_1_, "代名詞1", "biserial correlation"))
# 
# print(f(xiaosheng_1_, "代名詞2", "t-test"))
# print(f(xiaosheng_1_, "代名詞2", "biserial correlation"))
# 
# print(f(xiaosheng_1_, "代名詞3", "t-test"))
# print(f(xiaosheng_1_, "代名詞3", "biserial correlation"))

plot_df <- function(df, variable, ylab) {
    p <- ggplot(df, aes_string("高影響力", variable)) + 
        geom_boxplot(outlier.size=0.3) +
        stat_summary(fun=median, aes(group=0), geom="line", color="red") + 
        stat_summary(fun=median, geom="point", color="red", size=0.3) + 
        scale_x_discrete(labels=c("FALSE" = "low-influence", "TRUE" = "high-influence")) +
        xlab(NULL) +
        ylab(NULL) +
        labs(caption=ylab) +
        theme(plot.caption = element_text(hjust = 0.5))
    
    return(p)
    
}