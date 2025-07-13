(ns wiki-db.core
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.string :as str]
            [wiki-db.schema :as schema]
            [wiki-db.data :as data])
  (:gen-class))

(def db-spec
  {:dbtype "h2"
   :dbname "tcp://localhost:9092/~/wikidb"
   :user "sa"
   :password ""})

;; ============ ANALYTICS QUERIES ============

(defn get-all-text-content-alphabetically []
  "Get all text content from wiki sorted alphabetically"
  (jdbc/query db-spec
    ["SELECT 'users' as source_table, username as text_value FROM users
      UNION ALL
      SELECT 'categories' as source_table, name as text_value FROM categories
      UNION ALL
      SELECT 'pages' as source_table, title as text_value FROM pages
      UNION ALL
      SELECT 'page_tags' as source_table, tag as text_value FROM page_tags
      UNION ALL
      SELECT 'comments' as source_table, LEFT(content, 50) as text_value FROM comments
      ORDER BY text_value ASC"]))

(defn get-wiki-stats []
  "Get comprehensive wiki statistics"
  (let [stats (jdbc/query db-spec
                ["SELECT 
                    (SELECT COUNT(*) FROM users) as total_users,
                    (SELECT COUNT(*) FROM categories) as total_categories,
                    (SELECT COUNT(*) FROM pages) as total_pages,
                    (SELECT COUNT(*) FROM page_revisions) as total_revisions,
                    (SELECT COUNT(*) FROM page_links) as total_links,
                    (SELECT COUNT(*) FROM page_tags) as total_tags,
                    (SELECT COUNT(*) FROM comments) as total_comments,
                    (SELECT SUM(view_count) FROM pages) as total_views,
                    (SELECT SUM(word_count) FROM pages) as total_words"])]
    (first stats)))

(defn get-top-contributors []
  "Get users with most edits"
  (jdbc/query db-spec
    ["SELECT u.username, u.edit_count, u.is_admin,
             COUNT(p.id) as pages_created,
             COUNT(c.id) as comments_made
      FROM users u
      LEFT JOIN pages p ON u.id = p.created_by
      LEFT JOIN comments c ON u.id = c.user_id
      GROUP BY u.id, u.username, u.edit_count, u.is_admin
      ORDER BY u.edit_count DESC"]))

(defn get_most_popular_pages []
  "Get pages with most views"
  (jdbc/query db-spec
    ["SELECT p.title, p.view_count, p.word_count, c.name as category,
             u.username as author, p.created_at
      FROM pages p
      JOIN categories c ON p.category_id = c.id
      JOIN users u ON p.created_by = u.id
      ORDER BY p.view_count DESC
      LIMIT 10"]))

(defn get_content_by_category []
  "Analyze content distribution by category"
  (jdbc/query db-spec
    ["SELECT c.name as category, c.description,
             COUNT(p.id) as page_count,
             AVG(p.view_count) as avg_views,
             SUM(p.word_count) as total_words,
             MAX(p.view_count) as max_views
      FROM categories c
      LEFT JOIN pages p ON c.id = p.category_id
      GROUP BY c.id, c.name, c.description
      ORDER BY page_count DESC"]))

(defn get_tag_popularity []
  "Get most popular tags"
  (jdbc/query db-spec
    ["SELECT pt.tag, COUNT(*) as usage_count,
             STRING_AGG(p.title, ', ') as used_in_pages
      FROM page_tags pt
      JOIN pages p ON pt.page_id = p.id
      GROUP BY pt.tag
      ORDER BY usage_count DESC"]))

(defn search_content [search_term]
  "Search across all wiki content"
  (jdbc/query db-spec
    ["SELECT 'page' as content_type, p.title as title, p.slug,
             p.content, p.view_count, c.name as category
      FROM pages p
      JOIN categories c ON p.category_id = c.id
      WHERE LOWER(p.title) LIKE ? OR LOWER(p.content) LIKE ?
      UNION ALL
      SELECT 'comment' as content_type, 
             CONCAT('Comment on: ', p.title) as title, 
             p.slug,
             cm.content, 0 as view_count, 
             c.name as category
      FROM comments cm
      JOIN pages p ON cm.page_id = p.id
      JOIN categories c ON p.category_id = c.id
      WHERE LOWER(cm.content) LIKE ?
      ORDER BY view_count DESC"
     (str "%" (str/lower-case search_term) "%")
     (str "%" (str/lower-case search_term) "%")
     (str "%" (str/lower-case search_term) "%")]))

(defn get_link_analysis []
  "Analyze page linking patterns"
  (jdbc/query db-spec
    ["SELECT 
        p1.title as from_page,
        p2.title as to_page,
        pl.link_text,
        p1.view_count as from_views,
        p2.view_count as to_views
      FROM page_links pl
      JOIN pages p1 ON pl.from_page_id = p1.id
      JOIN pages p2 ON pl.to_page_id = p2.id
      ORDER BY p1.view_count DESC"]))

(defn get_word_count_analysis []
  "Analyze content by word count"
  (jdbc/query db-spec
    ["SELECT 
        p.title,
        p.word_count,
        p.view_count,
        c.name as category,
        CASE 
          WHEN p.word_count < 50 THEN 'Short'
          WHEN p.word_count < 100 THEN 'Medium'
          ELSE 'Long'
        END as content_length
      FROM pages p
      JOIN categories c ON p.category_id = c.id
      ORDER BY p.word_count DESC"]))

;; ============ DISPLAY FUNCTIONS ============

(defn print-wiki-stats []
  (println "\n🏛️  === WIKI STATISTICS ===")
  (let [stats (get-wiki-stats)]
    (println (format "👥 Total Users: %d" (:total_users stats)))
    (println (format "📂 Categories: %d" (:total_categories stats)))
    (println (format "📄 Pages: %d" (:total_pages stats)))
    (println (format "✏️  Revisions: %d" (:total_revisions stats)))
    (println (format "🔗 Links: %d" (:total_links stats)))
    (println (format "🏷️  Tags: %d" (:total_tags stats)))
    (println (format "💬 Comments: %d" (:total_comments stats)))
    (println (format "👀 Total Views: %,d" (:total_views stats)))
    (println (format "📝 Total Words: %,d" (:total_words stats)))))

(defn print-top-contributors []
  (println "\n🏆 === TOP CONTRIBUTORS ===")
  (doseq [user (get-top-contributors)]
    (println (format "%-15s | %3d edits | %2d pages | %2d comments %s"
                    (:username user)
                    (:edit_count user)
                    (:pages_created user)
                    (:comments_made user)
                    (if (:is_admin user) "👑" "")))))

(defn print-popular-pages []
  (println "\n🔥 === MOST POPULAR PAGES ===")
  (doseq [page (get_most_popular_pages)]
    (println (format "%-30s | %,4d views | %3d words | %-12s | by %s"
                    (:title page)
                    (:view_count page)
                    (:word_count page)
                    (:category page)
                    (:author page)))))

(defn print-category-analysis []
  (println "\n📊 === CONTENT BY CATEGORY ===")
  (doseq [cat (get_content_by_category)]
    (println (format "%-15s | %2d pages | %,4.0f avg views | %,5d words | max: %,d"
                    (:category cat)
                    (:page_count cat)
                    (or (:avg_views cat) 0)
                    (or (:total_words cat) 0)
                    (or (:max_views cat) 0)))))

(defn print-tag-popularity []
  (println "\n🏷️  === TAG POPULARITY ===")
  (doseq [tag (get_tag_popularity)]
    (println (format "%-15s | used %d times"
                    (:tag tag)
                    (:usage_count tag)))))

(defn print-all-text-alphabetically []
  (println "\n🔤 === ALL TEXT CONTENT (ALPHABETICAL) ===")
  (doseq [item (get-all-text-content-alphabetically)]
    (println (format "%-12s | %s"
                    (:source_table item)
                    (:text_value item)))))

(defn print-word-count-analysis []
  (println "\n📝 === WORD COUNT ANALYSIS ===")
  (doseq [page (get_word_count_analysis)]
    (println (format "%-30s | %3d words | %,4d views | %-6s | %s"
                    (:title page)
                    (:word_count page)
                    (:view_count page)
                    (:content_length page)
                    (:category page)))))

(defn print-search-results [term]
  (println (format "\n🔍 === SEARCH RESULTS FOR: '%s' ===" term))
  (let [results (search_content term)]
    (if (empty? results)
      (println "No results found")
      (doseq [result results]
        (println (format "%-8s | %-25s | %,4d views | %s"
                        (:content_type result)
                        (:title result)
                        (:view_count result)
                        (:category result)))))))

;; ============ MAIN FUNCTION ============

(defn setup-database! []
  (println "🏗️  Setting up wiki database...")
  (try
    (schema/create-schema! db-spec)
    (println "✅ Database schema created")
    
    ;; Check if data already exists
    (let [user-count (-> (jdbc/query db-spec ["SELECT COUNT(*) as count FROM users"])
                        first
                        :count)]
      (when (zero? user-count)
        (data/insert-sample-data! db-spec)))
    
    (println "✅ Database setup complete")
    true
    (catch Exception e
      (println "❌ Database setup failed:" (.getMessage e))
      false)))

(defn -main
  "Wiki Database Analytics"
  [& args]
  (println "🚀 === WIKI DATABASE ANALYTICS ===")
  (println "📊 Connecting to H2 database...")
  
  (if (setup-database!)
    (do
      (print-wiki-stats)
      (print-top-contributors)
      (print-popular-pages)
      (print-category-analysis)
      (print-tag-popularity)
      (print-word-count-analysis)
      (print-all-text-alphabetically)
      
      ;; Demo search
      (print-search-results "programming")
      (print-search-results "quantum")
      
      (println "\n🎉 Analysis complete!"))
    
    (println "❌ Failed to setup database. Make sure H2 TCP server is running on port 9092")))

;; ============ REPL HELPERS ============

(comment
  ;; Start H2 TCP server first:
  ;; java -cp ~/.m2/repository/com/h2database/h2/2.3.230/h2-2.3.230.jar org.h2.tools.Server -tcp -tcpPort 9092 -tcpAllowOthers
  
  ;; Then run these in REPL:
  (setup-database!)
  (print-wiki-stats)
  (print-search-results "functional")
  (get-all-text-content-alphabetically)
  
  ;; Interactive search
  (print-search-results "clojure")
  (print-search-results "war")
  (print-search-results "quantum"))
