use std::time::{SystemTime, UNIX_EPOCH};

pub struct PasswordHasher {
    pepper: [u8; 32], // Static application-wide secret
}

const K: [u32; 64] = [
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
];

impl PasswordHasher {
    pub fn new() -> Self {
        // In production, this should be loaded from a secure environment variable
        let pepper = [42u8; 32];
        PasswordHasher { pepper }
    }

    pub fn hash_password(&self, password: &str) -> String {
        let salt = self.generate_salt();
        let salted = format!("{}{}{}", salt, password, String::from_utf8_lossy(&self.pepper));
        
        let mut hash = self.sha256(salted.as_bytes());
        for _ in 0..10000 { // Key stretching
            hash = self.sha256(&hash);
        }

        // Combine salt and hash for storage
        format!("{}${}", salt, self.bytes_to_hex(&hash))
    }

    pub fn verify_password(&self, password: &str, hash_string: &str) -> bool {
        let parts: Vec<&str> = hash_string.split('$').collect();
        if parts.len() != 2 {
            return false;
        }

        let (salt, stored_hash) = (parts[0], parts[1]);
        let salted = format!("{}{}{}", salt, password, String::from_utf8_lossy(&self.pepper));
        
        let mut hash = self.sha256(salted.as_bytes());
        for _ in 0..10000 {
            hash = self.sha256(&hash);
        }

        self.bytes_to_hex(&hash) == stored_hash
    }

    fn generate_salt(&self) -> String {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .subsec_nanos();
        
        let mut salt = [0u8; 16];
        for i in 0..16 {
            salt[i] = ((nanos >> (i * 2)) & 0xff) as u8;
            salt[i] ^= self.pepper[i]; // Mix with pepper
        }
        
        self.bytes_to_hex(&salt)
    }

    // SHA-256 implementation
    fn sha256(&self, data: &[u8]) -> [u8; 32] {
        // Initial hash values (first 32 bits of the fractional parts of the square roots of the first 8 primes)
        let mut h0: u32 = 0x6a09e667;
        let mut h1: u32 = 0xbb67ae85;
        let mut h2: u32 = 0x3c6ef372;
        let mut h3: u32 = 0xa54ff53a;
        let mut h4: u32 = 0x510e527f;
        let mut h5: u32 = 0x9b05688c;
        let mut h6: u32 = 0x1f83d9ab;
        let mut h7: u32 = 0x5be0cd19;

        // Pre-processing
        let mut msg = data.to_vec();
        let original_len = msg.len() as u64 * 8;
        
        // Append the bit '1' to the message
        msg.push(0x80);
        
        // Append zeros
        while (msg.len() % 64) != 56 {
            msg.push(0);
        }
        
        // Append length
        msg.extend_from_slice(&original_len.to_be_bytes());

        // Process the message in 512-bit chunks
        for chunk in msg.chunks(64) {
            let mut w = [0u32; 64];

            // Break chunk into sixteen 32-bit big-endian words
            for i in 0..16 {
                w[i] = u32::from_be_bytes([
                    chunk[i * 4],
                    chunk[i * 4 + 1],
                    chunk[i * 4 + 2],
                    chunk[i * 4 + 3],
                ]);
            }

            // Extend the sixteen 32-bit words into sixty-four 32-bit words
            for i in 16..64 {
                let s0 = w[i-15].rotate_right(7) ^ w[i-15].rotate_right(18) ^ (w[i-15] >> 3);
                let s1 = w[i-2].rotate_right(17) ^ w[i-2].rotate_right(19) ^ (w[i-2] >> 10);
                w[i] = w[i-16].wrapping_add(s0).wrapping_add(w[i-7]).wrapping_add(s1);
            }

            // Initialize working variables
            let mut a = h0;
            let mut b = h1;
            let mut c = h2;
            let mut d = h3;
            let mut e = h4;
            let mut f = h5;
            let mut g = h6;
            let mut h = h7;

            // Main loop
            for i in 0..64 {
                let s1 = e.rotate_right(6) ^ e.rotate_right(11) ^ e.rotate_right(25);
                let ch = (e & f) ^ (!e & g);
                let temp1 = h.wrapping_add(s1).wrapping_add(ch).wrapping_add(K[i]).wrapping_add(w[i]);
                let s0 = a.rotate_right(2) ^ a.rotate_right(13) ^ a.rotate_right(22);
                let maj = (a & b) ^ (a & c) ^ (b & c);
                let temp2 = s0.wrapping_add(maj);

                h = g;
                g = f;
                f = e;
                e = d.wrapping_add(temp1);
                d = c;
                c = b;
                b = a;
                a = temp1.wrapping_add(temp2);
            }

            // Add the compressed chunk to the current hash value
            h0 = h0.wrapping_add(a);
            h1 = h1.wrapping_add(b);
            h2 = h2.wrapping_add(c);
            h3 = h3.wrapping_add(d);
            h4 = h4.wrapping_add(e);
            h5 = h5.wrapping_add(f);
            h6 = h6.wrapping_add(g);
            h7 = h7.wrapping_add(h);
        }

        // Produce the final hash value
        let mut hash = [0u8; 32];
        hash[0..4].copy_from_slice(&h0.to_be_bytes());
        hash[4..8].copy_from_slice(&h1.to_be_bytes());
        hash[8..12].copy_from_slice(&h2.to_be_bytes());
        hash[12..16].copy_from_slice(&h3.to_be_bytes());
        hash[16..20].copy_from_slice(&h4.to_be_bytes());
        hash[20..24].copy_from_slice(&h5.to_be_bytes());
        hash[24..28].copy_from_slice(&h6.to_be_bytes());
        hash[28..32].copy_from_slice(&h7.to_be_bytes());

        hash
    }

    fn bytes_to_hex(&self, bytes: &[u8]) -> String {
        bytes.iter()
            .map(|b| format!("{:02x}", b))
            .collect()
    }
} 