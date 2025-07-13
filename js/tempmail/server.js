const express = require('express');
const bodyParser = require('body-parser');
const SMTPServer = require('smtp-server').SMTPServer;
const { v4: uuidv4 } = require('uuid');
const tls = require('tls');
const fs = require('fs');

const app = express();
app.use(bodyParser.json());

// In-memory storage for emails
const emailStore = new Map();

// Rate limiting: Track the number of emails sent by each IP
const rateLimit = new Map();
const RATE_LIMIT = 10; // Max 10 emails per minute
const RATE_LIMIT_WINDOW = 60 * 1000; // 1 minute

// Generate a temporary email address
function generateTempEmail() {
    const randomString = Math.random().toString(36).substring(7);
    return `${randomString}@tempmail.com`;
}

// Validate email address format
function isValidEmail(email) {
    const regex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    return regex.test(email);
}

// SMTP server to receive emails
const smtpServer = new SMTPServer({
    authOptional: true, // Allow emails without authentication
    secure: false, // Set to true if using TLS
    onConnect(session, callback) {
        console.log(`[CONNECT] New connection from ${session.remoteAddress}`);
        callback();
    },
    onAuth(auth, session, callback) {
        console.log(`[AUTH] Attempted login by ${auth.username}`);
        if (auth.username === 'user' && auth.password === 'password') {
            console.log(`[AUTH] Successful login by ${auth.username}`);
            return callback(null, { user: auth.username });
        }
        console.log(`[AUTH] Failed login by ${auth.username}`);
        return callback(new Error('Invalid username or password'));
    },
    onMailFrom(address, session, callback) {
        console.log(`[MAIL FROM] Sender: ${address.address}`);
        callback();
    },
    onRcptTo(address, session, callback) {
        console.log(`[RCPT TO] Recipient: ${address.address}`);
        callback();
    },
    onData(stream, session, callback) {
        let emailData = '';
        console.log(`[DATA] Receiving email for ${session.envelope.rcptTo[0].address}`);
        stream.on('data', (chunk) => {
            emailData += chunk.toString();
        });
        stream.on('end', () => {
            const to = session.envelope.rcptTo[0].address;

            // Validate recipient email
            if (!isValidEmail(to)) {
                console.log(`[ERROR] Invalid recipient email: ${to}`);
                return callback(new Error('Invalid recipient email address'));
            }

            // Store the email
            if (emailStore.has(to)) {
                emailStore.get(to).push({ raw: emailData, timestamp: new Date() });
            }
            console.log(`[DATA] Email stored for ${to}`);
            callback();
        });
    },
    onError(err) {
        console.error(`[ERROR] SMTP Server Error: ${err.message}`);
    },
    onClose(session) {
        console.log(`[CLOSE] Connection closed by ${session.remoteAddress}`);
    },
    maxSize: 10 * 1024 * 1024, // Limit email size to 10MB
});

// Start the SMTP server on a non-privileged port
smtpServer.listen(2525, '0.0.0.0', () => {
    console.log('[SMTP] Server running on port 2525 (accessible from other networks)');
});

// Endpoint to create a temporary email address
app.post('/create-email', (req, res) => {
    const tempEmail = generateTempEmail();
    emailStore.set(tempEmail, []); // Initialize an empty array for emails
    console.log(`[API] Created temporary email: ${tempEmail}`);
    res.json({ email: tempEmail });
});

// Endpoint to retrieve emails for a specific temporary email address
app.get('/get-emails/:email', (req, res) => {
    const tempEmail = req.params.email;
    console.log(`[API] Retrieving emails for: ${tempEmail}`);

    if (!emailStore.has(tempEmail)) {
        console.log(`[API] Email not found: ${tempEmail}`);
        return res.status(404).json({ error: 'Temporary email address not found' });
    }

    const emails = emailStore.get(tempEmail);
    res.json({ emails });
});

// Endpoint to retrieve all emails
app.get('/emails', (req, res) => {
    console.log('[API] Retrieving all emails');
    const allEmails = {};
    emailStore.forEach((emails, address) => {
        allEmails[address] = emails;
    });
    res.json(allEmails);
});

// Endpoint to display server info
app.get('/', (req, res) => {
    console.log('[API] Serving server info');
    res.json({
        status: 'Running',
        smtpPort: 2525,
        apiPort: 3500,
        endpoints: {
            createEmail: 'POST /create-email',
            getEmails: 'GET /get-emails/:email',
            getAllEmails: 'GET /emails',
        },
    });
});

// Start the Express server
const PORT = 3500;
app.listen(PORT, () => {
    console.log(`[API] Temporary email API running on http://localhost:${PORT}`);
});
