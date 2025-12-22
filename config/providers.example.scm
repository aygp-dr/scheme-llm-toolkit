;;; Example Provider Configuration
;;; Copy this file to providers.scm and add your API keys

(define-module (config providers)
  #:export (ollama-config
            openai-config
            anthropic-config
            huggingface-config))

;; Ollama Configuration (Local Models)
(define ollama-config
  `((base-url . "http://localhost:11434")
    (models . ("llama2" "codellama" "mistral"))
    (timeout . 30000)
    (max-retries . 3)))

;; OpenAI Configuration
(define openai-config
  `((api-key . ,(or (getenv "OPENAI_API_KEY") "YOUR_OPENAI_API_KEY_HERE"))
    (organization . ,(getenv "OPENAI_ORG"))
    (base-url . "https://api.openai.com/v1")
    (models . ("gpt-4" "gpt-3.5-turbo" "gpt-4-turbo-preview" "gpt-4o" "gpt-4o-mini"))
    (default-model . "gpt-3.5-turbo")
    (max-tokens . 2000)
    (temperature . 0.7)
    (timeout . 300)))

;; Anthropic Configuration
(define anthropic-config
  `((api-key . ,(or (getenv "ANTHROPIC_API_KEY") "YOUR_ANTHROPIC_API_KEY_HERE"))
    (base-url . "https://api.anthropic.com/v1")
    (api-version . "2023-06-01")
    (models . ("claude-opus-4-20250514"
               "claude-sonnet-4-20250514"
               "claude-3-5-sonnet-20241022"
               "claude-3-5-haiku-20241022"
               "claude-3-opus-20240229"
               "claude-3-haiku-20240307"))
    (default-model . "claude-sonnet-4-20250514")
    (max-tokens . 4096)
    (timeout . 300)))

;; Hugging Face Configuration
(define huggingface-config
  `((api-key . ,(or (getenv "HUGGINGFACE_API_KEY") "YOUR_HUGGINGFACE_API_KEY_HERE"))
    (base-url . "https://api-inference.huggingface.co")
    (models . ("meta-llama/Llama-2-7b-chat-hf" 
               "mistralai/Mistral-7B-Instruct-v0.1"
               "google/flan-t5-xxl"))))

;; Default provider selection
(define default-provider 'ollama)