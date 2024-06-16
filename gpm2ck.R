library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(psych)

total_debt <- read_excel("C:/Users/Dell/Downloads/gpm2ck.xlsx", 
                         sheet = "total debt")
total_assets <- read_excel("C:/Users/Dell/Downloads/gpm2ck.xlsx", 
                           sheet = "total assets")
cash <- read_excel("C:/Users/Dell/Downloads/gpm2ck.xlsx", 
                           sheet = "cash")
sale <- read_excel("C:/Users/Dell/Downloads/gpm2ck.xlsx", 
                           sheet = "sale")
owner_capital <- read_excel("C:/Users/Dell/Downloads/gpm2ck.xlsx", 
                           sheet = "owner capital")
# Tạo dataframe
df <- merge(total_debt, total_assets, by = c("ID", "Date"))
df <- merge(df, cash, by = c("ID", "Date"))
df <- merge(df, sale, by = c("ID", "Date"))
df <- merge(df, owner_capital, by = c("ID", "Date"))
# Đổi tên cột 
colnames(df) <- c("ID", "Date", "Total_Debt", "Total_Assets", "Cash", "Sale", "Owner_Capital")
head(df)
summary(df)
sum(is.na(df))
df[is.na(df)] <- 0 
df$Date <- as.Date(df$Date, format="%d/%m/%Y")
# Kiểm tra kiểu dữ liệu của các cột
sapply(df, class)
# Chuyển đổi sang kiểu dữ liệu numeric
df$Cash <- as.numeric(df$Cash)
df$Sale <- as.numeric(df$Sale)
df$Owner_Capital <- as.numeric(df$Owner_Capital)
print(head(df))

df <- df %>%
  filter(Date >= as.Date("2015-12-31") & Date <= as.Date("2023-12-31"))

# Biểu đồ phân phối
#Total_Assets
p1 <- ggplot(df, aes(x = Date, y = Total_Assets)) +
  geom_line(color = "blue") +
  labs(title = "Phân phối của Tổng tài sản ở các quốc gia", x = "Ngày", y = "Total_Assets") +
  theme_minimal()
p1
#Total_Debt
p2 <- ggplot(df, aes(x = Date, y = Total_Debt)) +
  geom_line(color = "green") +
  labs(title = "Phân phối của Tổng nợ ở các quốc gia", x = "Ngày", y = "Total_Debt") +
  theme_minimal()
p2
#Cash
p3 <- ggplot(df, aes(x = Date, y = Cash)) +
  geom_line(color = "red") +
  labs(title = "Phân phối của Sở hữu tiền ở các quốc gia", x = "Ngày", y = "Cash") +
  theme_minimal()
p3
#Sale
p4 <- ggplot(df, aes(x = Date, y = Sale)) +
  geom_line(color = "orange") +
  labs(title = "Phân phối của Doanh thu ở các quốc gia", x = "Ngày", y = "Sale") +
  theme_minimal()
p4
#Owner_Capital
p5 <- ggplot(df, aes(x = Date, y = Owner_Capital)) +
  geom_line(color = "pink") +
  labs(title = "Phân phối của Vốn chủ sở hữu ở các quốc gia", x = "Ngày", y = "Owner_Capital") +
  theme_minimal()
p5

# Thống kê mô tả cho các biến độc lập
summary_statistics <- summary(df[, c("Cash", "Total_Debt", "Total_Assets", "Sale", "Owner_Capital")])
print(summary_statistics)
# Tính hệ số tương quan giữa các biến độc lập
cor_matrix <- cor(df[, c("Total_Assets", "Cash", "Total_Debt", "Sale", "Owner_Capital")])
print(cor_matrix)
df$Debt_TA <- df$Total_Debt / df$Total_Assets
#Biểu đồ tương quan
df1 <- df[, c("Total_Assets", "Cash", "Total_Debt", "Sale", "Owner_Capital", "Debt_TA")]
pairs.panels(df1)

#Xác định biến phụ thuộc và biến độc lập
X <- df[, c('Total_Assets', 'Cash', 'Total_Debt', 'Sale', 'Owner_Capital')]
Y <- df$Debt_TA

# Tạo và huấn luyện mô hình hồi quy OLS
model <- lm(Debt_TA ~ Total_Assets + Cash + Total_Debt + Sale + Owner_Capital, data = df)
summary(model)
# Hệ số hồi quy
coefficients <- summary(model)$coefficients
print(coefficients)

# Tóm tắt mô hình và lưu kết quả vào biến model_summary
model_summary <- summary(model)
print(model_summary)
# Giá trị P-value của F-statistic
f_statistic <- model_summary$fstatistic
f_p_value <- pf(f_statistic[1], f_statistic[2], f_statistic[3], lower.tail = FALSE)
print(paste("F-statistic p-value:", f_p_value))
# Đánh giá ý nghĩa thống kê của toàn bộ mô hình
if (f_p_value < 0.05) {
  print("Mô hình tổng thể có ý nghĩa thống kê.")
} else {
  print("Mô hình tổng thể không có ý nghĩa thống kê.")
}

# Tính toán phần dư
residuals <- residuals(model)
print("Phần dư (Residuals):")
print(residuals)
summary(residuals)

# Dự đoán giá trị từ mô hình
predictions <- predict(model, df)
# Tính toán sai số dự đoán
actuals <- df$Financial_Distress
residuals <- actuals - predictions
# Tính toán R-squared
r_squared <- summary(model)$r.squared
print(paste("R-squared:", r_squared))

#Tạo biến giả COVID
df$COVID <- ifelse(df$Date >= as.Date("2020-01-01"), 1, 0)
# Tạo và huấn luyện mô hình hồi quy tuyến tính bao gồm biến giả COVID
model_covid <- lm(Debt_TA ~ Total_Assets + Cash + Total_Debt + Sale + Owner_Capital + COVID, data = df)
summary(model_covid)
# Dự đoán mô hình
predictions_covid <- predict(model_covid, newdata = df)
# Tính R-squared
r2_covid <- summary(model_covid)$r.squared
print(paste('R-squared (with COVID):', r2_covid))
