;;; practical-conversations.scm --- Practical conversations for compaction testing

;; Copyright (C) 2025 aygp-dr
;; Licensed under MIT License

;;; Commentary:
;;;
;;; Practical conversation examples that any dev team can understand:
;;; - Technical support: Customer debugging an issue
;;; - Code review: Developer getting feedback
;;; - API integration: Questions about using an API
;;; - Incident response: Production issue troubleshooting
;;;
;;; Code:

;;; --------------------------------------------------------------------
;;; Technical Support Conversation
;;; --------------------------------------------------------------------

(define support-conversation
  '(((role . "system")
     (content . "You are a technical support engineer helping debug issues."))
    ((role . "user")
     (content . "My application keeps crashing with error code 500 when I try to upload files larger than 10MB."))
    ((role . "assistant")
     (content . "That sounds like a server-side issue. Error 500 typically indicates an internal server error. Let me help you debug this. First, can you check your server logs for the exact error message?"))
    ((role . "user")
     (content . "The logs show: PayloadTooLargeError: request entity too large"))
    ((role . "assistant")
     (content . "That confirms it. Your server has a request size limit configured. If you're using Express.js, you need to increase the body-parser limit. Add this configuration: app.use(express.json({limit: '50mb'}))"))
    ((role . "user")
     (content . "I added that but still getting the error. I'm using nginx as a reverse proxy."))
    ((role . "assistant")
     (content . "Nginx has its own limit. You need to set client_max_body_size in your nginx.conf. Add: client_max_body_size 50M; in the http, server, or location block. Then restart nginx with: sudo systemctl restart nginx"))
    ((role . "user")
     (content . "That fixed it! Files up to 50MB work now. Thanks!"))
    ((role . "assistant")
     (content . "Great! To summarize the fix: 1) Set Express body-parser limit to 50mb, 2) Set nginx client_max_body_size to 50M. For production, also consider implementing chunked uploads for very large files."))))

;; Key technical terms that must survive compaction
(define support-expected
  '("error 500"
    "PayloadTooLargeError"
    "body-parser"
    "client_max_body_size"
    "nginx"
    "50mb"))

;;; --------------------------------------------------------------------
;;; Code Review Conversation
;;; --------------------------------------------------------------------

(define code-review-conversation
  '(((role . "system")
     (content . "You are a senior developer conducting a code review."))
    ((role . "user")
     (content . "Can you review this function? def get_user(id): return db.query(f'SELECT * FROM users WHERE id = {id}')"))
    ((role . "assistant")
     (content . "This code has a critical SQL injection vulnerability. Never use f-strings to build SQL queries. An attacker could pass id=\"1; DROP TABLE users;--\" and destroy your database."))
    ((role . "user")
     (content . "Oh no, how do I fix it?"))
    ((role . "assistant")
     (content . "Use parameterized queries. Change to: db.query('SELECT * FROM users WHERE id = ?', [id]) or use an ORM like SQLAlchemy. This ensures the input is properly escaped."))
    ((role . "user")
     (content . "What about the SELECT * part?"))
    ((role . "assistant")
     (content . "Good catch. SELECT * is a code smell. It fetches unnecessary columns, breaks when schema changes, and exposes sensitive fields. Explicitly list needed columns: SELECT id, name, email FROM users"))
    ((role . "user")
     (content . "Should I add input validation too?"))
    ((role . "assistant")
     (content . "Yes, defense in depth. Validate that id is a positive integer before the query. Also add error handling: try/except around the query, return None or raise a custom UserNotFoundError if no results."))))

(define code-review-expected
  '("SQL injection"
    "parameterized"
    "SELECT *"
    "validation"
    "f-string"
    "ORM"))

;;; --------------------------------------------------------------------
;;; API Integration Conversation
;;; --------------------------------------------------------------------

(define api-conversation
  '(((role . "system")
     (content . "You are helping a developer integrate with a REST API."))
    ((role . "user")
     (content . "I'm getting 401 Unauthorized when calling the API. I set the API key in the URL like ?api_key=xxx"))
    ((role . "assistant")
     (content . "Most APIs require the key in the Authorization header, not the URL. Putting keys in URLs is insecure because they appear in logs and browser history. Use: Authorization: Bearer YOUR_API_KEY"))
    ((role . "user")
     (content . "Changed to header, now getting 429 Too Many Requests"))
    ((role . "assistant")
     (content . "You're hitting the rate limit. Check the response headers for X-RateLimit-Remaining and Retry-After. Implement exponential backoff: wait 1s, then 2s, then 4s between retries. Also consider caching responses."))
    ((role . "user")
     (content . "The API returns paginated results. How do I get all pages?"))
    ((role . "assistant")
     (content . "Look for pagination info in the response - usually next_page_token or a Link header. Loop until no more pages: while next_token: response = api.get(page_token=next_token); next_token = response.get('next_page_token')"))
    ((role . "user")
     (content . "Some calls are slow. Should I use async?"))
    ((role . "assistant")
     (content . "Yes, for multiple independent API calls use async/await with aiohttp or httpx. You can fetch pages concurrently with asyncio.gather(). But respect rate limits - add semaphores to limit concurrent requests."))))

(define api-expected
  '("Authorization"
    "Bearer"
    "429"
    "rate limit"
    "exponential backoff"
    "pagination"
    "async"))

;;; --------------------------------------------------------------------
;;; Incident Response Conversation
;;; --------------------------------------------------------------------

(define incident-conversation
  '(((role . "system")
     (content . "You are an SRE helping troubleshoot a production incident."))
    ((role . "user")
     (content . "URGENT: Production is down. Users getting 503 errors. Started 10 minutes ago."))
    ((role . "assistant")
     (content . "Acknowledged. First, check service health: kubectl get pods -n production. Look for pods in CrashLoopBackOff or not Ready. Also check: kubectl top nodes for resource exhaustion."))
    ((role . "user")
     (content . "3 of 5 pods are CrashLoopBackOff. Logs show: OOMKilled"))
    ((role . "assistant")
     (content . "Memory exhaustion. Immediate fix: increase memory limits in deployment. Run: kubectl set resources deployment/api -c api --limits=memory=2Gi. This will trigger rolling restart."))
    ((role . "user")
     (content . "Pods restarting but crashing again quickly"))
    ((role . "assistant")
     (content . "There's likely a memory leak. Check recent deployments: kubectl rollout history deployment/api. If a recent deploy caused this, rollback: kubectl rollout undo deployment/api"))
    ((role . "user")
     (content . "Rolled back to previous version. Pods stable now. What's the RCA?"))
    ((role . "assistant")
     (content . "Root cause: memory leak introduced in latest deploy caused OOMKilled. Immediate action: rollback. Follow-up: 1) Review diff of failed deploy, 2) Add memory monitoring alerts, 3) Load test before deploy, 4) Update runbook with this scenario."))))

(define incident-expected
  '("503"
    "CrashLoopBackOff"
    "OOMKilled"
    "memory"
    "rollback"
    "kubectl"
    "RCA"))

;;; --------------------------------------------------------------------
;;; Onboarding/Tutorial Conversation
;;; --------------------------------------------------------------------

(define onboarding-conversation
  '(((role . "system")
     (content . "You are onboarding a new developer to the codebase."))
    ((role . "user")
     (content . "I just joined the team. Where do I start?"))
    ((role . "assistant")
     (content . "Welcome! First, clone the repo and run make setup - this installs dependencies and creates your .env file. Check README.md for architecture overview."))
    ((role . "user")
     (content . "Setup done. How do I run tests?"))
    ((role . "assistant")
     (content . "Run make test for unit tests, make test-integration for integration tests (needs Docker running). We use pytest with coverage - aim for 80% minimum on new code."))
    ((role . "user")
     (content . "How do I deploy changes?"))
    ((role . "assistant")
     (content . "We use GitFlow. Create feature branch from develop: git checkout -b feature/your-feature. Push and create PR. After code review and CI passes, merge to develop. Develop auto-deploys to staging."))
    ((role . "user")
     (content . "What about production?"))
    ((role . "assistant")
     (content . "Production deploys happen via release branches. Weekly, we cut a release from develop, test on staging, then merge to main. Main auto-deploys to production. Hotfixes branch from main directly."))))

(define onboarding-expected
  '("make setup"
    "make test"
    "pytest"
    "GitFlow"
    "feature branch"
    "staging"
    "production"))

;;; --------------------------------------------------------------------
;;; Collection of all practical conversations
;;; --------------------------------------------------------------------

(define practical-conversations
  `((support . ((messages . ,support-conversation)
                (expected . ,support-expected)
                (description . "Technical support debugging file upload issue")
                (key-info . "nginx config, express limits, error codes")))

    (code-review . ((messages . ,code-review-conversation)
                    (expected . ,code-review-expected)
                    (description . "Code review finding SQL injection vulnerability")
                    (key-info . "security fix, parameterized queries")))

    (api . ((messages . ,api-conversation)
            (expected . ,api-expected)
            (description . "API integration troubleshooting")
            (key-info . "auth headers, rate limits, pagination")))

    (incident . ((messages . ,incident-conversation)
                 (expected . ,incident-expected)
                 (description . "Production incident response")
                 (key-info . "k8s commands, OOM diagnosis, rollback")))

    (onboarding . ((messages . ,onboarding-conversation)
                   (expected . ,onboarding-expected)
                   (description . "New developer onboarding")
                   (key-info . "setup, testing, deployment workflow")))))

;;; practical-conversations.scm ends here
