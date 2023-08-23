#install.packages('httr')

library(httr)

key = readLines(file.choose())

# Set the API endpoint
endpoint <- "https://api.openai.com/v1/chat/completions"

# Define the prompt you want to send to the model
prompt <- "Tell me a joke"

# Define the data payload
data <- list(
  prompt = prompt,
  max_tokens = 50,
  model = 'gpt-3.5-turbo'
)

# Send the POST request to the API
response <- POST(
  url = endpoint,
  body = data,
  add_headers(Authorization = paste("Bearer", key))
)

# Get the response content
content <- content(response, as = "text")
cat(content)
