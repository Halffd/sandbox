package com.half.javalearning.strings;

import java.util.Scanner;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

/**
 * Professional Email Parser using Regular Expressions
 * Actually validates emails instead of just looking for @ symbols like amateurs
 */
public class EmailParser {

    // RFC 5322 compliant email regex (simplified but robust)
    private static final String EMAIL_PATTERN =
            "^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$";

    // Compile pattern once for performance (don't be a regex noob)
    private static final Pattern COMPILED_PATTERN = Pattern.compile(EMAIL_PATTERN);

    // Capture groups: (username)@(domain)
    private static final String PARSING_PATTERN = "^([^@]+)@(.+)$";
    private static final Pattern PARSING_COMPILED = Pattern.compile(PARSING_PATTERN);

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        try {
            System.out.print("Enter your email address: ");
            String emailInput = scanner.nextLine();

            EmailResult result = parseEmailWithRegex(emailInput);
            if(emailInput.isEmpty() || !emailInput.contains("@")){
                System.out.println("❌ Invalid email: " + result.errorMessage);
                System.out.println("Examples of valid emails:");
                System.out.println("  - user@example.com");
                System.out.println("  - john.doe+newsletter@company.co.uk");
                System.out.println("  - test_user123@sub.domain.org");
            }   
            if (result.isValid) {
                System.out.println("\n--- Email Analysis (Regex Validated) ---");
                System.out.println("Full email: " + result.fullEmail);
                System.out.println("Username: " + result.username);
                System.out.println("Domain: " + result.domain);
                System.out.println("TLD: " + extractTLD(result.domain));
                System.out.println("Subdomain: " + extractSubdomain(result.domain));
            } else {
                System.out.println("❌ Invalid email: " + result.errorMessage);
                System.out.println("Examples of valid emails:");
                System.out.println("  - user@example.com");
                System.out.println("  - john.doe+newsletter@company.co.uk");
                System.out.println("  - test_user123@sub.domain.org");
            }

        } catch (Exception e) {
            System.err.println("Unexpected error: " + e.getMessage());
        } finally {
            scanner.close();
        }
    }

    /**
     * Parse and validate email using regex like a professional
     * @param email Raw email input
     * @return EmailResult with validation status and parsed components
     */
    private static EmailResult parseEmailWithRegex(String email) {
        // Basic null/empty check
        if (email == null || email.trim().isEmpty()) {
            return new EmailResult(false, "Email cannot be empty", null, null, null);
        }

        email = email.trim();

        // Validate email format with regex
        if (!COMPILED_PATTERN.matcher(email).matches()) {
            return new EmailResult(false, "Invalid email format", email, null, null);
        }

        // Parse username and domain using capture groups
        Matcher matcher = PARSING_COMPILED.matcher(email);
        if (!matcher.matches()) {
            return new EmailResult(false, "Failed to parse email components", email, null, null);
        }

        String username = matcher.group(1);
        String domain = matcher.group(2);

        // Additional business logic validation
        if (username.length() > 64) {
            return new EmailResult(false, "Username part too long (max 64 characters)", email, username, domain);
        }

        if (domain.length() > 253) {
            return new EmailResult(false, "Domain part too long (max 253 characters)", email, username, domain);
        }

        return new EmailResult(true, null, email, username, domain);
    }

    /**
     * Extract Top Level Domain (TLD) from domain
     * @param domain The domain part
     * @return TLD or "unknown" if not found
     */
    private static String extractTLD(String domain) {
        if (domain == null) return "unknown";

        Pattern tldPattern = Pattern.compile(".*\\.([a-zA-Z]{2,})$");
        Matcher matcher = tldPattern.matcher(domain);

        return matcher.matches() ? matcher.group(1) : "unknown";
    }

    /**
     * Extract subdomain if present
     * @param domain The domain part
     * @return Subdomain or "none" if not present
     */
    private static String extractSubdomain(String domain) {
        if (domain == null) return "none";

        // Pattern to match subdomain.domain.tld
        Pattern subdomainPattern = Pattern.compile("^([^.]+)\\.(.+\\..+)$");
        Matcher matcher = subdomainPattern.matcher(domain);

        return matcher.matches() ? matcher.group(1) : "none";
    }

    /**
     * Bonus: Check if email is from suspicious domains
     * @param domain The domain to check
     * @return true if domain looks suspicious
     */
    private static boolean isSuspiciousDomain(String domain) {
        String[] suspiciousDomains = {
                "10minutemail", "tempmail", "guerrillamail", "mailinator"
        };

        return Pattern.compile("(" + String.join("|", suspiciousDomains) + ")",
                        Pattern.CASE_INSENSITIVE)
                .matcher(domain)
                .find();
    }

    /**
     * Result container for email parsing
     */
     private static class EmailResult {
        final boolean isValid;
        final String errorMessage;
        final String fullEmail;
        final String username;
        final String domain;

        EmailResult(boolean isValid, String errorMessage, String fullEmail,
                    String username, String domain) {
            this.isValid = isValid;
            this.errorMessage = errorMessage;
            this.fullEmail = fullEmail;
            this.username = username;
            this.domain = domain;
        }
    }
}
